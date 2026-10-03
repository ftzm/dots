{
  lib,
  runCommand,
  python3,
  nerd-fonts,
  # About two pixels at 16pt / 96 DPI: align the symbols with 17pt Iosevka caps.
  raiseEm ? 0.10,
}: let
  fontTools = python3.withPackages (ps: [ps.fonttools]);
  source = nerd-fonts.symbols-only;
in
  runCommand "symbols-nerd-fonts-ftzm-${source.version}" {
    nativeBuildInputs = [fontTools];
    meta = {
      description = "Nerd symbol fonts with symbols raised for Iosevka";
      inherit (source.meta) license platforms;
    };
  } ''
    mkdir -p "$out/share/fonts/truetype"
    for variant in "" Mono; do
      python - "${source}/share/fonts/truetype/NerdFonts/Symbols/SymbolsNerdFont$variant-Regular.ttf" \
        "$out/share/fonts/truetype/SymbolsNerdFont''${variant}Ftzm-Regular.ttf" \
        ${lib.escapeShellArg (toString raiseEm)} <<'PY'
    import sys
    from fontTools.ttLib import TTFont

    font = TTFont(sys.argv[1], recalcBBoxes=False, recalcTimestamp=False)
    lift = round(float(sys.argv[3]) * font["head"].unitsPerEm)
    # Move each mapped glyph once (some codepoints share a glyph). Preserve the
    # relative positioning of symbols, including intentionally asymmetric ones.
    for name in set(font.getBestCmap().values()):
        glyph = font["glyf"][name]
        if not glyph.numberOfContours:
            continue
        if glyph.isComposite() or glyph.program.getBytecode():
            raise ValueError(f"Expected unhinted, simple symbols; review upstream glyph {name}")
        glyph.coordinates.translate((0, lift))
        glyph.recalcBounds(font["glyf"])

    # Update the overall ink bounds without changing the font's line metrics.
    visible = [font["glyf"][name] for name in font.getGlyphOrder()
               if font["glyf"][name].numberOfContours]
    font["head"].yMin = min(g.yMin for g in visible)
    font["head"].yMax = max(g.yMax for g in visible)

    # A distinct family avoids collisions with packaged or locally installed fonts.
    # Preserve copyright/license records, advance widths, and line metrics.
    family = font["name"].getBestFamilyName() + " ftzm"
    postscript_name = family.replace(" ", "") + "-Regular"
    names = {
        1: family, 2: "Regular", 3: family + " " + font["name"].getDebugName(5),
        4: family + " Regular", 6: postscript_name,
        16: family, 17: "Regular", 18: family + " Regular",
        21: family, 22: "Regular",
    }
    for name_id, value in names.items():
        font["name"].removeNames(nameID=name_id)
        font["name"].setName(value, name_id, 3, 1, 0x409)
        font["name"].setName(value, name_id, 1, 0, 0)
    font.save(sys.argv[2])
    PY
    done
  ''
