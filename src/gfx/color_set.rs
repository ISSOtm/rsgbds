/* SPDX-License-Identifier: MPL-2.0 */

use std::{cmp::Ordering, fmt::Display};

use arrayvec::ArrayVec;
use plumers::color::Rgb16;

use crate::rgb::Rgba;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ColorSet {
    /// A sorted array of CGB colors.
    colors: ArrayVec<Rgb16, 4>,
}

impl ColorSet {
    pub fn new() -> Self {
        Self {
            colors: ArrayVec::new(),
        }
    }

    /// Returns whether the color was unique to the set.
    pub fn add(&mut self, color: Rgb16) -> bool {
        let mut i = 0;
        // Look for where the color should be inserted to keep the array sorted.
        for other in &self.colors {
            match color.cmp(other) {
                Ordering::Greater => {}          // Keep searching.
                Ordering::Equal => return false, // Found it!
                Ordering::Less => break,         // `color` should be inserted before `other`.
            }
            i += 1;
        }

        // Insert the color.
        self.colors.try_insert(i, color).is_ok()
    }

    pub fn len(&self) -> usize {
        self.colors.len()
    }

    pub fn is_empty(&self) -> bool {
        self.colors.is_empty()
    }

    pub fn iter(&self) -> std::slice::Iter<'_, Rgb16> {
        self.colors.iter()
    }

    pub fn contains(&self, color: Rgb16) -> bool {
        self.colors.iter().any(|&candidate| candidate == color)
    }
}

/// The sorting is in the sense of set inclusion.
impl PartialOrd for ColorSet {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let is_sorted_and_unique = |slice: &[_]| {
            slice
                .windows(2)
                .enumerate()
                .find_map(|(i, pair)| (pair[0] >= pair[1]).then_some(i))
        };
        // We are able to scan forward through the lists only because they are sorted.
        debug_assert_eq!(is_sorted_and_unique(&self.colors), None);
        debug_assert_eq!(is_sorted_and_unique(&other.colors), None);

        let mut ours = self.colors.iter();
        let mut theirs = other.colors.iter();

        // Find the first non-matching entry.
        for (&our, &their) in std::iter::zip(ours.by_ref(), theirs.by_ref()) {
            match our.cmp(&their) {
                Ordering::Equal => {} // Keep searching.
                Ordering::Less => {
                    return check_if_contained(theirs, ours).map(|()| Ordering::Greater);
                }
                Ordering::Greater => {
                    return check_if_contained(ours, theirs).map(|()| Ordering::Less);
                }
            }
        }

        // Ensures that `subset` is indeed a subset of `superset`.
        fn check_if_contained<'a, It: Iterator<Item = &'a Rgb16>>(
            subset: It,
            mut superset: It,
        ) -> Option<()> {
            for item in subset {
                // Look for that item in `superset`.
                // Any entries lower than `item` can be skipped, but any entry higher than it means
                // that it won't be found ever (because the sets are sorted).
                loop {
                    // If the superset is exhausted, then we won't find `item`.
                    let candidate = superset.next()?;
                    match candidate.cmp(item) {
                        Ordering::Less => continue,       // Keep looking for `item`.
                        Ordering::Equal => break,         // We found `item`!
                        Ordering::Greater => return None, // Failed to match.
                    }
                }
            }
            // All elements in `subset` were matched!
            Some(())
        }

        // One of the groups is a prefix of the other.
        // The shortest one is contained within ("smaller than") the other.
        self.colors.len().partial_cmp(&other.colors.len())
    }
}

pub(crate) fn format_color_set<'a, It: IntoIterator<Item = &'a Rgb16>>(
    iter: It,
    f: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result {
    struct Color(Rgb16);
    impl Display for Color {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self.0 {
                Rgba::TRANSPARENT => write!(f, "transparent"),
                Rgb16(color) => write!(f, "${color:04x}"),
            }
        }
    }

    write!(f, "[")?;
    let mut colors = iter.into_iter();
    if let Some(first) = colors.next() {
        write!(f, "{}", Color(*first))?;
        for color in colors {
            write!(f, ", {}", Color(*color))?;
        }
    }
    write!(f, "]")
}

impl Display for ColorSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        format_color_set(&self.colors, f)
    }
}
