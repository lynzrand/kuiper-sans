# Kuiper Sans (仮)

Kuiper Sans is a font built from code. The font is inspired by Belleve Invis's [Iosevka][]. Always want to do that. Unsure if this project will ever finish.

The name "Kuiper Sans" is currently a placeholder.

[iosevka]: https://github.com/be5invis/iosevka

## Progress

- Support library
  - [x] TTF Required tables
    - [x] cmap - Character to glyph mapping
    - [x] head - Font header
    - [x] hhea - Horizontal header
    - [x] hmtx - Horizontal metrics
    - [x] maxp - Maximum profile
    - [x] name - Naming table
    - [x] OS/2 - OS/2 and Windows specific metrics
    - [x] post - PostScript information
  - [ ] Tables Related to TrueType Outlines
    - [ ] cvt - Control Value Table (optional table)
    - [ ] fpgm - Font program (optional table)
    - [x] glyf - Glyph data
    - [x] loca - Index to location
    - [ ] prep - Control Value Program (optional table)
    - [ ] gasp - Grid-fitting/Scan-conversion (optional table)
  - [ ] Math and stroke functions
- [ ] Font itself
  - [ ] ASCII range

## Building

You will need a relatively new F# compiler to build the font. Other than that, it's just `dotnet run --project Rynco.KuiperSans`.

## Developing

`opentype.js` is used to verify the correctness of the font face output. You will need `pnpm` to install dependencies and `node` to run the verification scripts.

## Shortcuts

If you have `just` installed, you can run:

- `just build` for building
- `just verify` for verifying the output

## License

Please do patiently wait until this thing is at least usable.
