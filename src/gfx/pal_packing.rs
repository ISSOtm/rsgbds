/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * SPDX-License-Identifier: MPL-2.0
 */

// The solvers here are picked from the paper at https://arxiv.org/abs/1605.00558:
// "Algorithms for the Pagination Problem, a Bin Packing with Overlapping Items"
// Their formulation of the problem consists in packing "tiles" into "pages"; here is a
// correspondence table for our application of it:
// Paper | rgbgfx
// ------+-------
//  Tile | Color set
//  Page | Palette

use std::collections::{HashSet, VecDeque};

use bitvec::prelude::*;
use plumers::color::Rgb16;

use crate::{color_set::ColorSet, Options};

pub(crate) fn pack_palettes(
    color_sets: &[ColorSet],
    options: &Options,
    has_transparency: bool,
) -> (Vec<usize>, usize) {
    // Sorting the color sets by size improves the packing algorithm's efficiency.
    let mut sorted_set_ids = Vec::from_iter(0..color_sets.len());
    sorted_set_ids.sort_unstable_by_key(|&idx| color_sets[idx].len());

    let mut queue = VecDeque::from_iter(sorted_set_ids.into_iter().map(ColorSetAttrs::new));
    let mut assignments = Vec::<AssignedSets>::new();

    while let Some(attrs) = queue.pop_front() {
        let set = &color_sets[attrs.set_idx];
        let (best_pal_index, _best_rel_size) = assignments
            .iter()
            .enumerate()
            .filter(|(i, _assignment)| !attrs.is_banned_from(*i))
            .fold(
                // We're looking for a palette where the color set's relative size is less than its
                // actual size; `assignments.len()` is merely a sentinel value for below.
                (assignments.len(), set.len() as f64),
                |(best_pal_index, best_rel_size), (i, assignment)| {
                    // Can't use `std::min_by_key` because floats are not `Ord`.
                    // TODO: use `OrdFloat` instead.
                    let rel_size = assignment.relative_size_of(set, color_sets);
                    if rel_size < best_rel_size {
                        (i, rel_size)
                    } else {
                        (best_pal_index, best_rel_size)
                    }
                },
            );

        if let Some(best_pal) = assignments.get_mut(best_pal_index) {
            // Add the color set to that palette.
            best_pal.assign(attrs);

            // If this overloads the palette, get it back to normal (if possible).
            while best_pal.volume(color_sets) > options.colors_per_palette(has_transparency).into()
            {
                // Look for a set minimising "efficiency" (i.e. size / rel_size).
                let efficiency =
                    |set: &ColorSet| set.len() as f64 / best_pal.relative_size_of(set, color_sets);

                let mut efficiencies =
                    best_pal.assigned.iter().enumerate().filter_map(|(i, opt)| {
                        opt.as_ref()
                            .map(|attrs| (i, efficiency(&color_sets[attrs.set_idx])))
                    });
                let Some((zero, first_efficiency)) = efficiencies.next() else {
                    unreachable!();
                };
                let (
                    mut least_efficient_idx,
                    mut smallest_efficiency,
                    mut most_efficient_idx,
                    mut largest_efficiency,
                ) = (zero, first_efficiency, zero, first_efficiency);

                for (i, efficiency) in efficiencies {
                    // TODO: use `OrdFloat` and `std::min/max`?
                    if efficiency < smallest_efficiency {
                        least_efficient_idx = i;
                        smallest_efficiency = efficiency;
                    }
                    if efficiency > largest_efficiency {
                        most_efficient_idx = i;
                        largest_efficiency = efficiency;
                    }
                }

                // All efficiencies are identical if and only if the min equals the max;
                // in which case the conditions above will never have been fulfilled, and thus the
                // indices themselves will be identical (to zero).
                if least_efficient_idx == most_efficient_idx {
                    // No option to fix the palette's overloading is better than the others...
                    break;
                }

                // Remove the color set with minimal efficiency.
                let mut min_efficiency_set = best_pal.assigned[least_efficient_idx].take().unwrap();
                min_efficiency_set.ban_from(best_pal_index); // Ban it from this palette, to ensure the algorithm terminates.
                queue.push_back(min_efficiency_set);
            }
        } else {
            // Found nowhere to put it, create a new palette containing just that set.
            assignments.push(AssignedSets::with_attrs([attrs]));
        }
    }

    // Deal with palettes still overloaded, by emptying them into the queue.
    for palette in &mut assignments {
        if palette.volume(color_sets) > options.colors_per_palette(has_transparency).into() {
            for attrs in &mut palette.assigned.iter_mut().filter_map(Option::take) {
                queue.push_back(attrs);
            }
            // The palette is now empty, but don't clear its backing storage, because we're about to re-fill the palettes anyway.
        }
    }
    // Place back any color sets now in the queue, via first-fit.
    while let Some(attrs) = queue.pop_front() {
        let set = &color_sets[attrs.set_idx];
        match assignments
            .iter_mut()
            .find(|palette| palette.can_fit(set, color_sets, options, has_transparency))
        {
            Some(palette) => palette.assign(attrs),
            None => assignments.push(AssignedSets::with_attrs([attrs])),
        }
    }

    // "Decant" the result.
    decant(&mut assignments, color_sets, options, has_transparency);
    // The result doesn't contain any empty palettes.
    debug_assert_eq!(
        assignments.iter().position(AssignedSets::is_empty),
        None,
        "{assignments:?}",
    );

    let mut mappings = vec![0; color_sets.len()];
    for (i, palette) in assignments.iter().enumerate() {
        for attrs in palette.iter() {
            mappings[attrs.set_idx] = i;
        }
    }

    (mappings, assignments.len())
}

fn decant(
    assignments: &mut Vec<AssignedSets>,
    color_sets: &[ColorSet],
    options: &Options,
    has_transparency: bool,
) {
    // "Decanting" is the process of moving all "things" that can fit in a lower index there.
    fn decant_on<F: Fn(&mut AssignedSets, &mut AssignedSets)>(
        assignments: &mut Vec<AssignedSets>,
        try_decanting: F,
    ) {
        // No need to attempt decanting on palette #0, as there are no palettes to decant to.
        for from in (1..assignments.len()).rev() {
            // Scan all palettes before this one.
            let (candidates, scanned) = assignments.split_at_mut(from);
            for candidate in candidates {
                // TODO: if `from` is now empty, then there's no point in continuing the iteration.
                try_decanting(candidate, &mut scanned[0]);
            }

            // If the color set is now empty, remove it.
            // Doing this now reduces the number of iterations performed by later steps.
            // NB: order is intentionally preserved, so as not to alter the "decantation"'s properties.
            // NB: this does mean that the first step might get empty sets as its inputs!
            // NB: this is safe to do, because we iterate towards the beginning of the vector, so
            //     we won't be skipping elements.
            if assignments[from].is_empty() {
                assignments.remove(from);
            }
        }
    }

    // Decant on palettes.
    decant_on(assignments, |to, from| {
        if to.combined_volume(
            from.iter()
                .flat_map(|set| color_sets[set.set_idx].iter().copied()),
            color_sets,
        ) <= options.colors_per_palette(has_transparency).into()
        {
            for opt in from.assigned.iter_mut() {
                if let Some(attrs) = opt.take() {
                    to.assign(attrs);
                }
            }
            from.clear();
        }
    });

    // Decant on "components" (= color sets sharing colors).
    decant_on(assignments, |to, from| {
        // We need to iterate on all the "components", which are groups of color sets sharing at
        // least one color with another color set in the group.
        // We do this by adding the first available color set, and then looking for sets
        // with common colors. (As an optimization, we know we can skip sets already scanned.)
        let mut processed = BitVec::<usize, Lsb0>::repeat(false, from.len());
        let mut colors = HashSet::new();
        let mut members = Vec::with_capacity(processed.len()); // Indices of the "component"'s members.

        // While some sets still have yet to be processed...
        while let Some(idx) = processed.first_zero() {
            // Build up the "component"...
            colors.clear();
            members.clear();
            for ((i, attrs), mut was_processed) in from
                .assigned
                .iter()
                .enumerate()
                .filter_map(|(i, opt)| opt.as_ref().map(|attrs| (i, attrs)))
                .skip(idx)
                .zip(processed[idx..].iter_mut())
            {
                let color_set = &color_sets[attrs.set_idx];
                // FIXME: annoyingly, this is O(n²).
                if members.is_empty() || colors.iter().any(|&color| color_set.contains(color)) {
                    colors.extend(color_set.iter());
                    members.push(i);
                    was_processed.set(true);
                }
            }

            if to.combined_volume(colors.iter().copied(), color_sets)
                <= options.colors_per_palette(has_transparency).into()
            {
                // Iterate through the component's color sets, and transfer them.
                for &i in &members {
                    to.assign(from.assigned[i].take().unwrap());
                }
            }
        }
    });

    // Decant on individual color sets.
    decant_on(assignments, |to, from| {
        for slot in &mut from.assigned {
            if let Some(attrs) = slot.take() {
                if to.can_fit(
                    &color_sets[attrs.set_idx],
                    color_sets,
                    options,
                    has_transparency,
                ) {
                    to.assign(attrs);
                } else {
                    // Put the attribute back in.
                    *slot = Some(attrs);
                }
            }
        }
    })
}

/// A reference to a color set, and attached attributes for sorting purposes.
#[derive(Debug)]
struct ColorSetAttrs {
    set_idx: usize,
    banned_pages: BitVec,
}

/// A collection of color sets assigned to a palette.
/// Does not contain the actual color indices, because we need to be able to remove elements from it.
#[derive(Debug)]
struct AssignedSets {
    // We leave room for emptied slots, to avoid copying the structs around on removal.
    assigned: Vec<Option<ColorSetAttrs>>,
}

impl ColorSetAttrs {
    fn new(set_idx: usize) -> Self {
        Self {
            set_idx,
            banned_pages: Default::default(),
        }
    }

    fn is_banned_from(&self, page_idx: usize) -> bool {
        self.banned_pages
            .get(page_idx)
            .is_some_and(|bit| *bit.as_ref())
    }

    fn ban_from(&mut self, page_idx: usize) {
        if self.banned_pages.len() <= page_idx {
            self.banned_pages.resize(page_idx + 1, false);
        }
        self.banned_pages.set(page_idx, true);
    }
}

impl AssignedSets {
    fn with_attrs<It: IntoIterator<Item = ColorSetAttrs>>(attrs: It) -> Self {
        Self {
            assigned: attrs.into_iter().map(Some).collect(),
        }
    }

    fn iter(&self) -> impl Iterator<Item = &ColorSetAttrs> {
        self.assigned.iter().filter_map(Option::as_ref)
    }

    fn assign(&mut self, attrs: ColorSetAttrs) {
        match self.assigned.iter_mut().find(|opt| opt.is_none()) {
            Some(slot) => *slot = Some(attrs),
            None => self.assigned.push(Some(attrs)),
        }
    }

    fn clear(&mut self) {
        self.assigned.clear();
    }

    fn is_empty(&self) -> bool {
        self.assigned.iter().all(Option::is_none)
    }

    fn len(&self) -> usize {
        self.iter().count()
    }

    fn unique_colors(&self, color_sets: &[ColorSet]) -> HashSet<Rgb16> {
        self.iter()
            .flat_map(|attrs| color_sets[attrs.set_idx].iter())
            .copied()
            .collect()
    }

    fn volume(&self, color_sets: &[ColorSet]) -> usize {
        self.unique_colors(color_sets).len()
    }

    fn can_fit(
        &self,
        set: &ColorSet,
        color_sets: &[ColorSet],
        options: &Options,
        has_transparency: bool,
    ) -> bool {
        let mut unique_colors = self.unique_colors(color_sets);
        unique_colors.extend(set.iter().copied());
        unique_colors.len() <= options.colors_per_palette(has_transparency).into()
    }

    fn relative_size_of(&self, set: &ColorSet, color_sets: &[ColorSet]) -> f64 {
        set.iter().fold(0., |rel_size, &color| {
            let n = self
                .iter()
                .filter(|attrs| {
                    color_sets[attrs.set_idx]
                        .iter()
                        .any(|&candidate| candidate == color)
                })
                .count();
            // NOTE: The paper and the associated code disagree on this: the code has
            // this `1 +`, whereas the paper does not; its lack causes a division by 0
            // if the symbol is not found anywhere, so I'm assuming the paper is wrong.
            rel_size + 1. / (1 + n) as f64
        })
    }

    fn combined_volume<It: IntoIterator<Item = Rgb16>>(
        &self,
        iter: It,
        color_sets: &[ColorSet],
    ) -> usize {
        let mut unique_colors = self.unique_colors(color_sets);
        unique_colors.extend(iter);
        unique_colors.len()
    }
}

#[derive(Debug)]
struct OrdFloat(f64);

impl Ord for OrdFloat {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.total_cmp(&other.0)
    }
}
// Implement the rest in terms of the above, to avoid violating the consistency reauirements.
impl PartialOrd for OrdFloat {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl PartialEq for OrdFloat {
    fn eq(&self, other: &Self) -> bool {
        matches!(self.cmp(other), std::cmp::Ordering::Equal)
    }
}
impl Eq for OrdFloat {}
