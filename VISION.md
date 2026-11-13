# Vision

OrgWebAlchemy parses Org-mode with a real grammar and renders the resulting tree
to HTML, Markdown or SXML, on the bet that Org is a language rather than a pile of
lines matching regular expressions. The library is deliberately small — three
modules, no runtime dependencies beyond Guile — and its future is less about
adding exporters than about making the tree it produces trustworthy, documented
and fast enough to sit inside an editor.

## Where OrgWebAlchemy is today

At v1.2.9 the library is 2853 lines in three modules, and the split is the
architecture.

- `(orgwebalchemy peg)` — 1699 lines, the grammar and everything that is true
  about a link target: headings and their anchors, `figure-parts`,
  `named-parts`, `document-names`, `org-link-parts`/`protocol`/`path`/`search`,
  `org-image-link?`. It knows nothing about HTML or Markdown.
- `(orgwebalchemy html)` — 720 lines, rendering to an HTML fragment or SXML, with
  roughly thirty CSS class parameters so that a design system, not the library,
  decides what anything looks like. `heading-classes` is a procedure of the level;
  everything else is a list of class names. `sxml->html-string` is its own small
  serialiser.
- `(orgwebalchemy markdown)` — 434 lines from the same AST, falling back to inline
  HTML for underline, superscript and subscript, and to definition-list and
  footnote conventions where Markdown has them.

The supported surface is broad and honest about its edges: nested mixed lists and
checkbox items, tables with empty cells, source/example/quote/verse/center/export
blocks, keywords, comments, fixed-width lines, separators, footnotes, description
lists, figures with captions and names, named tables and blocks, `#custom-id` and
`*Heading` links, image links and `::search` suffixes. The known gaps are stated:
captions on non-figure elements are not rendered, there is no incremental parse,
and the benchmarks are not comparisons.

The testing is where this project is unusual, and it is the reason to trust the
tree: table-driven unit suites for the grammar and both renderers, snapshot tests
for whole documents, and two checks that compare a parser against a *previous* one
rather than against an expectation — an equivalence sweep over 70,858 inputs when
the run scanners were rewritten, and a stop-set proof that swept 391,094 positions
looking for a character that begins an inline construct without being in the set
the scanners leap over. The published benchmark tables report parse and render
time, allocation per operation, GC counts and the fragments handed to the collapse
pass.

Decisions that constrain everything else:

1. **Syntax in the parser, presentation in the renderers.** Anything a link,
   figure or name means is decided once, in `peg.scm`; the renderers only serialise.
2. **Run scanners and a stop set.** Speed comes from leaping over characters that
   cannot begin a construct, which means a new construct must also update
   `inline-stop-chars` — and the stop-set check proves it did.
3. **Narrow folding rules.** A caption or a name folds into an element only when
   it is adjacent and the element can carry an id, so the parser never invents a
   name that resolves to nothing.
4. **Markdown is deliberately not configurable.** It has no styling, so class
   parameters there would be a lie; where Markdown lacks a construct, inline HTML
   carries the meaning.
5. **A library, not an application.** No Emacs integration, no export dispatcher,
   no Babel, no file handling — strings in, strings out.

## The vision

1. **The supported surface is generated, not asserted.** The set of Org constructs
   the parser understands is a machine-readable statement derived from the grammar
   and exercised by a conformance suite shaped like the Org manual's element and
   object lists, so "does it read this?" is answered by running something.
2. **The AST is a documented, versioned vocabulary.** A stable node set with a
   schema and a JSON serialisation, so a third renderer — LaTeX, plain text,
   Gemini, a wiki's own tree — can be written without reading `peg.scm`.
3. **Parsing is incremental.** After an edit, a document is re-parsed from the
   smallest enclosing element rather than from the beginning, which is what it
   takes to sit inside an editor or a live preview.
4. **Captions are an attribute of an element, not a figure special case.** Tables,
   blocks and lists carry captions in both renderers, so the chart in a document
   is as addressable as the image in one.
5. **Performance is compared, not merely measured.** The same fixtures are parsed
   and rendered by this library and by at least one other Org implementation, and
   the numbers are in the document.

## Features that are in scope and not built yet

The README names the first two; the grammar already contains the seams for the
rest, and each has a hard part worth stating.

- **Captions on tables and blocks.** Today `#+CAPTION:` in front of a table stays
  an ordinary keyword line. Hard part: HTML only has `<caption>` for tables, so
  every other element needs a `<figure>` wrapper or a sibling element, and the two
  renderers have to agree on which — while Markdown has no caption at all and must
  fall back to the emphasis-plus-anchor trick figures already use.
- **Incremental parse and render.** The run scanners and the collapse pass assume
  a whole document. Hard part: caching element boundaries without holding stale
  offsets when text above the edit changes, and deciding what an "edit" even is
  when the caller hands over a string.
- **`#+INCLUDE`, `#+MACRO` and `#+OPTIONS`.** Real documents use all three. Hard
  part: `INCLUDE` needs file resolution, which a string-in/string-out library does
  not have — it needs a resolver parameter, and a story for what happens when the
  resolver fails.
- **Table alignment and widths.** Org writes `<l>`, `<c>`, `<r>` and `<10>` in a
  table's special row; nothing renders it. Hard part: Markdown has alignment (in
  the delimiter row) and HTML has `align`/`style`, and the two disagree about
  where the information lives.
- **Custom link protocols.** The parser classifies `http`, `file`, `#custom-id`,
  `*Heading` and bare names, and stops there. Hard part: a registry that lets a
  host application resolve `[[yt:…]]` or `[[zotero:…]]` without turning the parser
  into a configuration surface.
- **Non-HTML export blocks.** `#+BEGIN_EXPORT html` is trusted literal output;
  `latex` and others have no path. Hard part: what a Markdown renderer does with a
  LaTeX fragment — drop it loudly, or keep it and admit the output is not
  Markdown.
- **Source-block highlighting hooks.** Blocks carry a language and render a class.
  Hard part: staying a library while offering a hook a caller can bind to a
  highlighter, without making the default path depend on one.

## New features that would be worth having

- **A loss report.** `org-losses` (or similar) answering "what in this document
  could not be expressed in the target format?" is the honest API for a library
  that claims to keep meaning; it also gives the Markdown fallbacks a testable
  specification.
- **`org->json`.** The AST as data, for other languages, for snapshot testing and
  for the node vocabulary documentation. The SXML path proves the tree is
  serialisable; this makes the serialisation a contract.
- **A renderer for plain text or Gemini.** Both are closer to Org's own semantics
  than Markdown is, and either would prove the two-renderer agreement invariant is
  a property of the design rather than of the two modules that happen to exist.
- **A separate CLI.** `orgwebalchemy render file.org` is wanted by exactly the
  people who read the benchmarks, and it can live in `bin/` without making the
  library an application.
- **Markdown flavours.** Footnotes and underlines are handled one way today;
  GitHub-flavoured, CommonMark-strict and Pandoc-flavoured targets differ, and a
  profile argument would remove a class of "my renderer disagrees" reports.
- **A worked static-site example.** The library has one demo script; a small,
  complete site generator that reads a directory of Org files and writes HTML with
  a template would answer the most common question the README raises.

## Contributions that would move the needle

1. **Caption support for tables and blocks.** Visible, testable, shared by both
   renderers, and it removes the largest stated gap in the supported surface. The
   hard part is a wrapper decision that both renderers can live with.
2. **A loss report.** A small API with a large effect on trust: it makes the
   Markdown fallbacks explicit, and it gives every future construct a place to
   declare what it costs in a format that cannot express it.
3. **`org->json` plus node vocabulary documentation.** The change that turns the
   AST from an implementation detail into an interface, and the prerequisite for a
   third renderer written by somebody else.
4. **Benchmark comparisons.** The tables exist and the fixtures exist; what is
   missing is a second implementation measured on the same files. A single honest
   comparison table would settle more arguments than any optimisation.
5. **Table alignment.** Small, self-contained, and the kind of gap a user notices
   immediately because the output looks wrong rather than merely incomplete.
6. **An incremental-parse prototype.** The largest item here, and the one with the
   most design risk: worth a written design note before any code, because the
   collapse pass and the scanners both assume whole-document input.

## Compared to related projects

| Project | What it is | Where OrgWebAlchemy differs |
|---------|------------|------------------------------|
| Emacs `ox-html` and `ox-md` | Org's own exporters, the reference implementation of Org semantics, running inside Emacs | OrgWebAlchemy reproduces a subset of that semantics outside Emacs, from a PEG grammar, with no Babel, agenda or export dispatcher — and it can be embedded in a program that has no Emacs |
| Pandoc | a universal document converter with an Org reader and dozens of writers | Pandoc's centre of gravity is the interchange between formats; here Org is the only input, parsed to a tree that two renderers share, and Org-specific meaning (names, figures, anchors) is decided once rather than mapped generically |
| orgize | a Rust library that parses Org and exports HTML and LaTeX | the closest comparable in intent: differs in language and ecosystem, in the shape of its AST, and in being a compiled dependency rather than a Guile module |
| org-ruby | a Ruby Org parser and HTML exporter, used by forges to render `.org` files | one renderer, an older and partial grammar; OrgWebAlchemy aims at two renderers from one tree and at Org constructs org-ruby does not reach |
| uniorg and orgajs | JavaScript Org parsers in the unified ecosystem, producing hast or other trees with HTML and Markdown exporters | the JS ecosystem has the same two-renderer ambition; the difference is the grammar approach, the SXML output, and Guile's `(ice-9 peg)` as the whole parsing stack |
| nvim-orgmode | an Org implementation for Neovim: agenda, capture, folding, syntax | an editor, not a library, and it renders nothing outside its own buffer; a parser like this one could be its export backend, which is the kind of use the AST-as-interface work is for |

What the others do that this could still add: the coverage of the Org manual that
`ox-html` has by definition, Pandoc's breadth of output formats, and the ecosystem
reach that comes from being a library in a popular language. Where this is ahead
and should stay ahead: two renderers from one AST with a stated agreement
invariant, a grammar where the stop set is *proved* to cover every construct
rather than assumed, equivalence sweeps of tens of thousands of inputs published
with the change that needed them, per-element CSS class parameters, and a
benchmark table that reports allocation and fragments rather than only wall time.

## In the long run

- **Org outlives its editors.** The multi-year bet is that people will keep
  writing Org long after they stop agreeing about which program opens it, and that
  a correct, embeddable parser is the piece the ecosystem lacks outside Emacs.
- **The AST is the product.** Exporters are cheap once the tree is stable and
  documented; the durable asset is a node vocabulary other tools can target, which
  is why `org->json` and the vocabulary documentation outrank a fourth renderer.
- **Correctness is the differentiator.** The equivalence sweeps and the stop-set
  proof are unusual in a small library, and they are worth more than feature
  breadth: a parser that is *provably* unchanged is one people will embed in a
  wiki they cannot afford to break.

## Getting there from here

Captions first, because they are the largest stated gap and both renderers can be
fixed at once. Then the loss report, which makes the fallbacks a contract. Then
`org->json` and the node vocabulary, which is what turns the AST into an
interface. The benchmarks and an example site can be done at any time and pay for
themselves immediately; incremental parsing wants a design note before a line of
code.

*This document describes intent, not commitments: the exported procedures, the
stated limitations and the licence are the contract, and nothing here changes them.*
