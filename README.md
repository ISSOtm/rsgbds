# rsgbds — [RGBDS](https://rgbds.gbdev.io), but in Rust!

This is a continuation of RGBDS ([source code](https://github.com/gbdev/rgbds)), but written in Rust instead of C and C++.

## Motivation

I grew tired of RGBDS' [crashes](https://github.com/gbdev/rgbds/commit/2eca43cd2da5a4be21cada359bd5e152f1896453), uncleanliness, and [memory leaks](https://github.com/gbdev/rgbds/blob/2d15e405395912ea4e6bd3b3ce90010427ab44d8/src/asm/symbol.c#L295-L296).

Additionally, writing code in C requires to "spell everything out", which makes writing actual logic *very* painful.
[I][vec0] [have][vec1] [lost][vec2] [count][vec3] [of][vec4] [how][vec5] [many][vec6] [times][vec7] [I][vec8] [have][vec9] [reimplemented][vec10] [`std::vector`][vec11] [thus][vec12] [far][vec13]!
Some other functions, [I have had to reimplement myself][asprintf] in the name of portability (`asprintf` is only available as a GNU extension).

This forms the basis of my motivation for rewriting RGBDS in a language that isn't C.
The latter especially hinders implementation of new features.

The choice of Rust comes from the following pros:
- I am familiar with it.
- It's portable, likely on every system that RGBDS can already run on.
- It has (next to) no runtime, and no GC, so RGBDS will stay fast like an assembler ought to be.
- Rust appears to be more popular than C, so this should help new contributors.
- A lot of good libraries already exist to externalise a lot of tasks (holy crap, [`codespan-reporting`](https://lib.rs/crates/codespan-reporting)'s output makes me drool).
- The usual "no memory safety issues!" also applies.

I am also aware of the cons, but I believe Rust is the best compromise, at least for me.

## Status

RGBASM is the hardest program to rewrite of the four, for various reasons:
- it's *by far* the most complex;
- it is composed of a *lot* of interacting parts;
- it's entirely driver by the parser, so lifetimes are hard to track;
- oh and did I mention that it has a parser?

So, I am currently starting with RGBASM.
The design decisions I will have to take in it may heavily influence the other tools' code.

## Roadmap

- [ ] Rewrite RGBASM
- [ ] Integrate [`rgbds-obj`](https://github.com/ISSOtm/rgbds-obj) into a `rgbds` crate (which RGBLINK will use)
- [ ] Rewrite RGBLINK
- [ ] Rewrite RGBFIX
- [x] Rewrite RGBGFX
- [ ] Integrate [`rgbobj`](https://github.com/ISSOtm/rgbobj)
- [ ] Add an [xtask](https://github.com/matklad/cargo-xtask) for generating man pages from the CLI structs, warning list, and things like that. (This would avoid forgetting to document new CLI args, for example.) [This will be useful](https://docs.rs/syn/latest/syn/fn.parse_file.html).

Once all of this is done, cleanup time!

- [ ] Clean up / reorganise code:
  - [ ] Split modules more? Maybe one per `struct`/`enum`?
  - [ ] Reorganise e.g. `input.rs`, given how closely tied it is to `fstack.rs`?
        Maybe it'd be worth deepening the file tree a little more.
        (Make sure to add a `README.md` in each directory, to make the categorisation clearer.)
- [ ] Hand-write RGBASM's parser, to make all interfaces with the lexer saner:
  - No more `RefCell`s, the parser can "lend" everything to the `next()` method!
  - Sync points between the lexer and parser would be easier to control, eliminating the need for `lookahead_hack` (and associated productions).
  - Syntax error "expected bar, foo, or comma" can be customised (e.g. "expected instruction or directive"), though this is a bit of a double-edged sword.
  - No more `build.rs`, so, faster compile times!
  - Opens the door to revision-based grammar changes, to enhance back-compat (though I am far from ready for *that* rabbit hole yet...)
  - **Downside**: the grammar is likely to be less obvious, and e.g. conflicts won't be reported (implicitly, they will be solved statically).
    Oh well, such is life.

## License

This port of RGBDS is licensed under the Mozilla Public License, version 2.0.

> This Source Code Form is subject to the terms of the Mozilla Public License,
> v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain
> one at http://mozilla.org/MPL/2.0/.

The full text of the MPL v2.0 can be found in the `LICENSE` file.

[vec0]: https://github.com/gbdev/rgbds/blob/2d15e405395912ea4e6bd3b3ce90010427ab44d8/src/asm/charmap.c#L61-L69
[vec1]: https://github.com/gbdev/rgbds/blob/2d15e405395912ea4e6bd3b3ce90010427ab44d8/src/link/output.c#L85-L96
[vec2]: https://github.com/gbdev/rgbds/blob/2d15e405395912ea4e6bd3b3ce90010427ab44d8/src/link/output.c#L177-L188
[vec3]: https://github.com/gbdev/rgbds/blob/2d15e405395912ea4e6bd3b3ce90010427ab44d8/src/asm/parser.y#L274-L280
[vec4]: https://github.com/gbdev/rgbds/blob/2d15e405395912ea4e6bd3b3ce90010427ab44d8/src/asm/parser.y#L386-L392
[vec5]: https://github.com/gbdev/rgbds/blob/2d15e405395912ea4e6bd3b3ce90010427ab44d8/src/link/script.c#L43-L51
[vec6]: https://github.com/gbdev/rgbds/blob/2d15e405395912ea4e6bd3b3ce90010427ab44d8/src/link/script.c#L252-L258
[vec7]: https://github.com/gbdev/rgbds/blob/2d15e405395912ea4e6bd3b3ce90010427ab44d8/src/link/script.c#L268-L274
[vec8]: https://github.com/gbdev/rgbds/blob/2d15e405395912ea4e6bd3b3ce90010427ab44d8/src/asm/macro.c#L71-L79
[vec9]: https://github.com/gbdev/rgbds/blob/2d15e405395912ea4e6bd3b3ce90010427ab44d8/src/asm/rpn.c#L53-L64
[vec10]: https://github.com/gbdev/rgbds/blob/2d15e405395912ea4e6bd3b3ce90010427ab44d8/src/link/patch.c#L61-L65
[vec11]: https://github.com/gbdev/rgbds/blob/2d15e405395912ea4e6bd3b3ce90010427ab44d8/src/asm/lexer.c#L636-L647
[vec12]: https://github.com/gbdev/rgbds/blob/2d15e405395912ea4e6bd3b3ce90010427ab44d8/src/asm/symbol.c#L105-L111
[vec13]: https://github.com/gbdev/rgbds/blob/2d15e405395912ea4e6bd3b3ce90010427ab44d8/src/link/object.c#L140-L147
[asprintf]: https://github.com/gbdev/rgbds/blob/2d15e405395912ea4e6bd3b3ce90010427ab44d8/src/asm/fstack.c#L189-L204
