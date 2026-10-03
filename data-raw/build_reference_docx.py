#!/usr/bin/env python3
"""Build the ISLH Quarto reference .docx from Pandoc's default reference doc.

A Quarto/Pandoc "reference doc" is a .docx whose *styles* define the look of
rendered output; the body content is ignored. This script starts from Pandoc's
own reference.docx (so every style Pandoc emits is guaranteed to exist), then
applies Island Health branding:

  * the official ISLH Office theme, lifted verbatim from
    templates/letterhead-standard.dotx (BC Sans + the ISLH colour scheme, so
    Word's colour picker shows the same swatches as official ISLH templates)
  * heading/body/caption/link styles matching the letterhead
  * a header carrying the horizontal ISLH logo, and a "Page X of Y" footer
  * Letter page size with the letterhead's margins

Usage:
    python3 tools/build_reference_docx.py

Requires: pandoc on PATH (to supply the base reference doc).
Output:   templates/islh-report-reference.docx
"""

from __future__ import annotations

import shutil
import struct
import subprocess
import sys
import zipfile
import json
import re
from pathlib import Path

PKG = Path(__file__).resolve().parent.parent
HERE = Path(__file__).resolve().parent

# Vendored from islh-brand-standard so the 3.6 MB letterhead template does not
# have to ride along in this repository:
#   theme1.xml  <- word/theme/theme1.xml inside templates/letterhead-standard.dotx
#   logo        <- logos/2. Horizontal Logo/1. Full-Colour (primary)/...png
# Refresh both from that repo if the official letterhead changes.
THEME = HERE / "theme1.xml"
LOGO = HERE / "logo-horizontal-full-colour.png"
TOKENS = PKG / "inst" / "brand" / "tokens.json"

# The reference doc ships inside the Quarto extension, which is copied into a
# user's project as a unit, so it lives there rather than in a second location.
OUTPUT = (
    PKG / "inst" / "quarto" / "_extensions" / "islh" / "islh-report"
    / "islh-report-reference.docx"
)

W = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
EMU_PER_INCH = 914400

# --- Brand values -----------------------------------------------------------
# Sourced from the ISLH brand standards (guidelines/colour.mhtml) and the
# official letterhead theme. Sizes are half-points, spacing is twips.
#
# BLUE_DARK is the letterhead's heading colour and the theme's "text2" slot, so
# headings are declared against the theme rather than hardcoded - that keeps
# this doc visually identical to official ISLH Word templates.
# Read rather than retyped, so this file cannot drift from the R colour table
# or from the official Office theme.
#
# BLUE_DARK and LINK_BLUE come from the letterhead theme's own colour scheme
# (the "dk2" and "hlink" slots), not from the brand ramp. They sit one or two
# steps off the nearest ramp value, and matching the theme is what keeps this
# document visually identical to official ISLH Word templates.
def _theme_colour(slot):
    scheme = re.search(r"<a:clrScheme.*?</a:clrScheme>", THEME.read_text(), re.S)
    hit = re.search(
        r"<a:" + slot + r">\s*<a:(?:srgbClr val=\"([0-9A-Fa-f]{6})\""
        r"|sysClr[^/]*lastClr=\"([0-9A-Fa-f]{6})\")",
        scheme.group(0),
    )
    if hit is None:
        raise SystemExit(f"theme1.xml has no colour in the {slot} slot")
    return (hit.group(1) or hit.group(2)).lower()


def _ramp(family, value):
    tokens = json.loads(TOKENS.read_text())
    return tokens["families"][family][str(value)].lstrip("#").lower()


BLUE_DARK = _theme_colour("dk2")      # headings; theme text2
LINK_BLUE = _theme_colour("hlink")    # theme hyperlink slot
GREY_20 = _ramp("grey", 20)           # captions
GREY_30 = _ramp("grey", 30)           # footer, subtitle, metadata
BLUE_96 = _ramp("blue", 96)           # table header fill
GREY_85 = _ramp("grey", 85)           # table borders

# Body/caption/footer greys are chosen to satisfy the brand's own contrast
# rule: text below 18px needs a 70+ colour-value difference from its
# background. Against white (value 100) that means value 30 or darker, which
# rules out the mid greys for 9pt caption and footer text.

LOGO_WIDTH_IN = 1.9

# --- Page geometry (twips) --------------------------------------------------
# Letter, with the letterhead's 1418-twip (2.5cm) margins.
PAGE_WIDTH = 12240
PAGE_HEIGHT = 15840
MARGIN = 1418
TEXT_WIDTH = PAGE_WIDTH - 2 * MARGIN  # 9404
TAB_CENTER = TEXT_WIDTH // 2
TAB_RIGHT = TEXT_WIDTH


def png_size(path: Path) -> tuple[int, int]:
    """Return (width, height) in pixels for a PNG, read from its IHDR."""
    header = path.read_bytes()[:33]
    if header[:8] != b"\x89PNG\r\n\x1a\n":
        raise ValueError(f"not a PNG: {path}")
    width, height = struct.unpack(">II", header[16:24])
    return width, height


def read_zip_member(archive: Path, name: str) -> bytes:
    with zipfile.ZipFile(archive) as zf:
        return zf.read(name)


def base_reference_docx(dest: Path) -> None:
    """Ask pandoc for its default reference.docx."""
    if not shutil.which("pandoc"):
        sys.exit(
            "pandoc not found on PATH. Install pandoc (or use the copy bundled "
            "with Quarto) and re-run."
        )
    with dest.open("wb") as fh:
        subprocess.run(
            ["pandoc", "--print-default-data-file", "reference.docx"],
            stdout=fh,
            check=True,
        )


# --- Style definitions ------------------------------------------------------
# Each entry replaces the <w:pPr>/<w:rPr> of the named style. Styles inherit
# the BC Sans theme font from theme1.xml, so fonts are only set where a style
# needs to deviate (i.e. monospace).


def heading(size: int, before: int, after: int, italic: bool = False) -> str:
    ital = "<w:i/>" if italic else ""
    return (
        f'<w:pPr><w:keepNext/><w:spacing w:before="{before}" w:after="{after}" '
        f'w:line="240" w:lineRule="auto"/></w:pPr>'
        f'<w:rPr><w:b/>{ital}<w:color w:val="{BLUE_DARK}" w:themeColor="text2"/>'
        f'<w:sz w:val="{size}"/><w:szCs w:val="{size}"/></w:rPr>'
    )


PARAGRAPH_STYLES: dict[str, str] = {
    # Body: 11pt, 1.15 line spacing (the letterhead's docDefaults), with a
    # little more paragraph spacing than the letter template since reports run
    # much longer.
    "Normal": (
        '<w:pPr><w:spacing w:before="0" w:after="120" w:line="276" '
        'w:lineRule="auto"/></w:pPr>'
        '<w:rPr><w:sz w:val="22"/><w:szCs w:val="22"/></w:rPr>'
    ),
    "Title": (
        '<w:pPr><w:keepNext/><w:spacing w:before="0" w:after="120" '
        'w:line="240" w:lineRule="auto"/><w:contextualSpacing/></w:pPr>'
        f'<w:rPr><w:b/><w:color w:val="{BLUE_DARK}" w:themeColor="text2"/>'
        '<w:spacing w:val="-10"/><w:kern w:val="28"/>'
        '<w:sz w:val="56"/><w:szCs w:val="56"/></w:rPr>'
    ),
    # Subtitle is basedOn Title in pandoc's reference doc, so bold/kerning and
    # the heading colour are inherited unless explicitly switched off.
    "Subtitle": (
        '<w:pPr><w:spacing w:before="0" w:after="240"/>'
        "<w:contextualSpacing/></w:pPr>"
        f'<w:rPr><w:b w:val="0"/><w:color w:val="{GREY_30}"/>'
        '<w:spacing w:val="0"/><w:kern w:val="0"/>'
        '<w:sz w:val="32"/><w:szCs w:val="32"/></w:rPr>'
    ),
    "Author": (
        '<w:pPr><w:spacing w:before="0" w:after="60"/></w:pPr>'
        f'<w:rPr><w:color w:val="{GREY_30}"/><w:sz w:val="22"/></w:rPr>'
    ),
    "Date": (
        '<w:pPr><w:spacing w:before="0" w:after="240"/></w:pPr>'
        f'<w:rPr><w:color w:val="{GREY_30}"/><w:sz w:val="22"/></w:rPr>'
    ),
    # Heading sizes mirror the letterhead: 22 / 18 / 15 / 14 / 12 / 11 pt.
    "Heading1": heading(44, 240, 120),
    "Heading2": heading(36, 240, 120),
    "Heading3": heading(30, 200, 100),
    "Heading4": heading(28, 200, 100),
    "Heading5": heading(24, 160, 80),
    "Heading6": heading(22, 160, 80, italic=True),
    "Caption": (
        '<w:pPr><w:spacing w:before="80" w:after="200" w:line="240" '
        'w:lineRule="auto"/></w:pPr>'
        f'<w:rPr><w:i/><w:iCs/><w:color w:val="{GREY_20}"/>'
        '<w:sz w:val="18"/><w:szCs w:val="18"/></w:rPr>'
    ),
    # Pandoc puts an image in a "Figure" paragraph and its caption in the
    # "ImageCaption" paragraph that follows. Without keepNext on Figure, a page
    # break can fall between them and the caption is orphaned at the top of the
    # next page, describing a figure the reader cannot see.
    "Figure": (
        '<w:pPr><w:keepNext/><w:spacing w:before="120" w:after="0"/>'
        "</w:pPr>"
    ),
    # Quarto uses ImageCaption before the image when DOCX captions are on top.
    # keepNext moves both to the next page when the figure no longer fits;
    # keepLines also stops a two-line caption splitting across a break.
    "ImageCaption": (
        '<w:pPr><w:keepNext/><w:keepLines/>'
        '<w:spacing w:before="80" w:after="200" '
        'w:line="240" w:lineRule="auto"/></w:pPr>'
        f'<w:rPr><w:i/><w:iCs/><w:color w:val="{GREY_20}"/>'
        '<w:sz w:val="18"/><w:szCs w:val="18"/></w:rPr>'
    ),
    # The caption sits above its table, so it must keep with what follows.
    "TableCaption": (
        '<w:pPr><w:keepNext/><w:keepLines/><w:spacing w:before="80" '
        'w:after="80" w:line="240" w:lineRule="auto"/></w:pPr>'
        f'<w:rPr><w:i/><w:iCs/><w:color w:val="{GREY_20}"/>'
        '<w:sz w:val="18"/><w:szCs w:val="18"/></w:rPr>'
    ),
    # Block quotes: a blue rule on the left rather than the brand's discouraged
    # decorative treatments.
    "BlockText": (
        '<w:pPr><w:pBdr><w:left w:val="single" w:sz="18" w:space="10" '
        f'w:color="{BLUE_DARK}"/></w:pBdr>'
        '<w:ind w:left="360"/><w:spacing w:before="120" w:after="120"/></w:pPr>'
        f'<w:rPr><w:i/><w:iCs/><w:color w:val="{GREY_30}"/></w:rPr>'
    ),
    "AbstractTitle": (
        '<w:pPr><w:keepNext/><w:spacing w:before="240" w:after="80"/></w:pPr>'
        f'<w:rPr><w:b/><w:color w:val="{BLUE_DARK}" w:themeColor="text2"/>'
        '<w:sz w:val="24"/></w:rPr>'
    ),
    "FootnoteText": (
        '<w:pPr><w:spacing w:before="0" w:after="0" w:line="240" '
        'w:lineRule="auto"/></w:pPr>'
        '<w:rPr><w:sz w:val="18"/><w:szCs w:val="18"/></w:rPr>'
    ),
    "TOCHeading": heading(36, 240, 120),
}

CHARACTER_STYLES: dict[str, str] = {
    "Hyperlink": (
        f'<w:rPr><w:color w:val="{LINK_BLUE}" w:themeColor="hyperlink"/>'
        '<w:u w:val="single"/></w:rPr>'
    ),
    # BC Sans has no monospace companion; fall back to the standard Windows /
    # macOS mono faces.
    "VerbatimChar": (
        '<w:rPr><w:rFonts w:ascii="Consolas" w:hAnsi="Consolas" '
        'w:cs="Consolas"/><w:sz w:val="20"/><w:szCs w:val="20"/>'
        '<w:shd w:val="clear" w:color="auto" w:fill="f3f4f4"/></w:rPr>'
    ),
}

# Styles Pandoc's reference doc doesn't define but a report needs.
# Tab stops are derived from the real text width (see TAB_CENTER/TAB_RIGHT)
# rather than Word's 1"-margin defaults, since this template uses the
# letterhead's 1418-twip margins.
EXTRA_STYLES = f"""
  <w:style w:type="paragraph" w:styleId="Header">
    <w:name w:val="header"/><w:basedOn w:val="Normal"/><w:uiPriority w:val="99"/>
    <w:pPr><w:tabs><w:tab w:val="center" w:pos="{TAB_CENTER}"/>
    <w:tab w:val="right" w:pos="{TAB_RIGHT}"/></w:tabs>
    <w:spacing w:before="0" w:after="0" w:line="240" w:lineRule="auto"/></w:pPr>
    <w:rPr><w:color w:val="{GREY_30}"/><w:sz w:val="18"/></w:rPr>
  </w:style>
  <w:style w:type="paragraph" w:styleId="Footer">
    <w:name w:val="footer"/><w:basedOn w:val="Normal"/><w:uiPriority w:val="99"/>
    <w:pPr><w:tabs><w:tab w:val="center" w:pos="{TAB_CENTER}"/>
    <w:tab w:val="right" w:pos="{TAB_RIGHT}"/></w:tabs>
    <w:spacing w:before="0" w:after="0" w:line="240" w:lineRule="auto"/></w:pPr>
    <w:rPr><w:color w:val="{GREY_30}"/><w:sz w:val="18"/></w:rPr>
  </w:style>
  <w:style w:type="paragraph" w:styleId="SourceCode">
    <w:name w:val="Source Code"/><w:basedOn w:val="Normal"/>
    <w:pPr><w:spacing w:before="60" w:after="60" w:line="240" w:lineRule="auto"/>
    <w:shd w:val="clear" w:color="auto" w:fill="f3f4f4"/>
    <w:ind w:left="120" w:right="120"/></w:pPr>
    <w:rPr><w:rFonts w:ascii="Consolas" w:hAnsi="Consolas" w:cs="Consolas"/>
    <w:sz w:val="20"/><w:szCs w:val="20"/></w:rPr>
  </w:style>
"""

# Table style: ISLH light-blue header band (theme lt2) with dark blue text.
TABLE_STYLE = f"""
  <w:style w:type="table" w:styleId="Table">
    <w:name w:val="Table"/><w:uiPriority w:val="99"/>
    <w:tblPr>
      <w:tblBorders>
        <w:top w:val="single" w:sz="4" w:space="0" w:color="{GREY_85}"/>
        <w:bottom w:val="single" w:sz="4" w:space="0" w:color="{GREY_85}"/>
        <w:insideH w:val="single" w:sz="4" w:space="0" w:color="{GREY_85}"/>
      </w:tblBorders>
      <w:tblCellMar>
        <w:top w:w="80" w:type="dxa"/><w:left w:w="108" w:type="dxa"/>
        <w:bottom w:w="80" w:type="dxa"/><w:right w:w="108" w:type="dxa"/>
      </w:tblCellMar>
    </w:tblPr>
    <w:tblStylePr w:type="firstRow">
      <w:rPr><w:b/><w:color w:val="{BLUE_DARK}" w:themeColor="text2"/></w:rPr>
      <w:tcPr>
        <w:shd w:val="clear" w:color="auto" w:fill="{BLUE_96}"/>
        <w:tcBorders>
          <w:bottom w:val="single" w:sz="8" w:space="0" w:color="{BLUE_DARK}"/>
        </w:tcBorders>
      </w:tcPr>
    </w:tblStylePr>
  </w:style>
"""


def restyle(styles_xml: str) -> str:
    """Apply the ISLH style definitions to Pandoc's styles.xml."""
    from lxml import etree

    root = etree.fromstring(styles_xml.encode("utf-8"))

    def qn(tag: str) -> str:
        return f"{{{W}}}{tag}"

    by_id = {
        s.get(qn("styleId")): s for s in root.findall(qn("style"))
    }

    def apply(style_id: str, fragment: str) -> None:
        style = by_id.get(style_id)
        if style is None:
            print(f"  ! style {style_id} not in base reference doc; skipped")
            return
        # Drop existing direct formatting, then append ours.
        for tag in ("pPr", "rPr"):
            for node in style.findall(qn(tag)):
                style.remove(node)
        wrapper = etree.fromstring(
            f'<w:wrap xmlns:w="{W}">{fragment}</w:wrap>'.encode("utf-8")
        )
        for child in wrapper:
            style.append(child)

    for style_id, fragment in PARAGRAPH_STYLES.items():
        apply(style_id, fragment)
    for style_id, fragment in CHARACTER_STYLES.items():
        apply(style_id, fragment)

    # Replace the table style outright, then add the styles pandoc lacks.
    table = by_id.get("Table")
    if table is not None:
        root.remove(table)
    additions = etree.fromstring(
        f'<w:wrap xmlns:w="{W}">{TABLE_STYLE}{EXTRA_STYLES}</w:wrap>'.encode("utf-8")
    )
    for child in additions:
        root.append(child)

    return etree.tostring(root, xml_declaration=True, encoding="UTF-8", standalone=True).decode()


# --- Header / footer / page setup -------------------------------------------


def header_xml(logo_cx: int, logo_cy: int) -> str:
    """Header with the horizontal ISLH logo, right-aligned."""
    return f"""<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<w:hdr xmlns:w="{W}"
       xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
       xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
       xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
       xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture">
  <w:p>
    <w:pPr><w:pStyle w:val="Header"/><w:jc w:val="right"/></w:pPr>
    <w:r>
      <w:drawing>
        <wp:inline distT="0" distB="0" distL="0" distR="0">
          <wp:extent cx="{logo_cx}" cy="{logo_cy}"/>
          <wp:effectExtent l="0" t="0" r="0" b="0"/>
          <wp:docPr id="1" name="Island Health logo" descr="Island Health"/>
          <wp:cNvGraphicFramePr>
            <a:graphicFrameLocks noChangeAspect="1"/>
          </wp:cNvGraphicFramePr>
          <a:graphic>
            <a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/picture">
              <pic:pic>
                <pic:nvPicPr>
                  <pic:cNvPr id="1" name="Island Health logo" descr="Island Health"/>
                  <pic:cNvPicPr/>
                </pic:nvPicPr>
                <pic:blipFill>
                  <a:blip r:embed="rId1"/>
                  <a:stretch><a:fillRect/></a:stretch>
                </pic:blipFill>
                <pic:spPr>
                  <a:xfrm>
                    <a:off x="0" y="0"/>
                    <a:ext cx="{logo_cx}" cy="{logo_cy}"/>
                  </a:xfrm>
                  <a:prstGeom prst="rect"><a:avLst/></a:prstGeom>
                </pic:spPr>
              </pic:pic>
            </a:graphicData>
          </a:graphic>
        </wp:inline>
      </w:drawing>
    </w:r>
  </w:p>
</w:hdr>
"""


def _field(instr: str) -> str:
    """A Word field code run (e.g. PAGE, NUMPAGES)."""
    return (
        '<w:r><w:fldChar w:fldCharType="begin"/></w:r>'
        f'<w:r><w:instrText xml:space="preserve"> {instr} </w:instrText></w:r>'
        '<w:r><w:fldChar w:fldCharType="separate"/></w:r>'
        "<w:r><w:t>1</w:t></w:r>"
        '<w:r><w:fldChar w:fldCharType="end"/></w:r>'
    )


# Two <w:tab/>s, not one: paragraph tab stops merge with the style's rather
# than replacing them, so a single tab lands on the centre stop. Stepping
# through both puts the page number hard against the right margin.
FOOTER_XML = f"""<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<w:ftr xmlns:w="{W}"
       xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">
  <w:p>
    <w:pPr><w:pStyle w:val="Footer"/></w:pPr>
    <w:r><w:t xml:space="preserve">Island Health</w:t></w:r>
    <w:r><w:tab/><w:tab/><w:t xml:space="preserve">Page </w:t></w:r>
    {_field("PAGE")}
    <w:r><w:t xml:space="preserve"> of </w:t></w:r>
    {_field("NUMPAGES")}
  </w:p>
</w:ftr>
"""

# Page setup. Child order follows the OOXML schema: header/footer references,
# then footnote/endnote properties, then page geometry.
SECT_PR_HEAD = """<w:sectPr>
  <w:headerReference w:type="default" r:id="rIdHeader1"/>
  <w:footerReference w:type="default" r:id="rIdFooter1"/>"""

SECT_PR_TAIL = f"""
  <w:pgSz w:w="{PAGE_WIDTH}" w:h="{PAGE_HEIGHT}"/>
  <w:pgMar w:top="{MARGIN}" w:right="{MARGIN}" w:bottom="{MARGIN}" w:left="{MARGIN}"
           w:header="851" w:footer="709" w:gutter="0"/>
  <w:cols w:space="720"/>
  <w:docGrid w:linePitch="326"/>
</w:sectPr>"""

# Pandoc's default reference doc used to carry an empty self-closing
# <w:sectPr/>; since pandoc 3.8 it carries a populated one holding footnote
# settings. Replace the page geometry either way, but carry across the
# footnote and endnote properties rather than silently dropping them.
CARRIED_SECT_CHILDREN = ("w:footnotePr", "w:endnotePr")


def build_sect_pr(existing_inner: str) -> str:
    carried = []
    for tag in CARRIED_SECT_CHILDREN:
        hit = re.search(rf"<{tag}>.*?</{tag}>|<{tag}\s*/>", existing_inner, re.S)
        if hit:
            carried.append("  " + hit.group(0).strip())

    leftover = existing_inner
    for tag in CARRIED_SECT_CHILDREN:
        leftover = re.sub(rf"<{tag}>.*?</{tag}>|<{tag}\s*/>", "", leftover, flags=re.S)
    leftover = leftover.strip()
    if leftover:
        # A future pandoc adding something new should be looked at, not
        # discarded on the quiet.
        print(f"  note: dropping unrecognised sectPr content: {leftover[:200]}")

    return SECT_PR_HEAD + ("\n" + "\n".join(carried) if carried else "") + SECT_PR_TAIL


HEADER_RELS = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
  <Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" Target="media/islh-logo.png"/>
</Relationships>
"""


def build() -> None:
    if not THEME.exists():
        sys.exit(f"missing {THEME}")
    if not LOGO.exists():
        sys.exit(f"missing {LOGO}")

    work = HERE / ".build-reference-docx.tmp"
    base = work / "base.docx"
    work.mkdir(exist_ok=True)
    try:
        print("Fetching pandoc's default reference.docx ...")
        base_reference_docx(base)

        print("Reading the official ISLH Office theme ...")
        theme = THEME.read_bytes()

        px_w, px_h = png_size(LOGO)
        logo_cx = int(LOGO_WIDTH_IN * EMU_PER_INCH)
        logo_cy = int(logo_cx * px_h / px_w)
        print(f"Logo {px_w}x{px_h}px -> {LOGO_WIDTH_IN}in wide in the header")

        with zipfile.ZipFile(base) as zf:
            parts = {name: zf.read(name) for name in zf.namelist()}

        print("Applying ISLH styles ...")
        parts["word/styles.xml"] = restyle(
            parts["word/styles.xml"].decode("utf-8")
        ).encode("utf-8")
        parts["word/theme/theme1.xml"] = theme

        # Page setup + header/footer wiring. Replace whichever shape of
        # <w:sectPr> this pandoc emits rather than adding a second one.
        document = parts["word/document.xml"].decode("utf-8")
        populated = re.search(r"<w:sectPr(?:\s[^>]*)?>(.*?)</w:sectPr>", document, re.S)
        empty = re.search(r"<w:sectPr(?:\s[^>]*)?/>", document)
        if populated:
            document = document.replace(populated.group(0), build_sect_pr(populated.group(1)))
        elif empty:
            document = document.replace(empty.group(0), build_sect_pr(""))
        else:
            document = document.replace("</w:body>", f"{build_sect_pr('')}</w:body>")
        parts["word/document.xml"] = document.encode("utf-8")

        parts["word/header1.xml"] = header_xml(logo_cx, logo_cy).encode("utf-8")
        parts["word/footer1.xml"] = FOOTER_XML.encode("utf-8")
        parts["word/_rels/header1.xml.rels"] = HEADER_RELS.encode("utf-8")
        parts["word/media/islh-logo.png"] = LOGO.read_bytes()

        rels = parts["word/_rels/document.xml.rels"].decode("utf-8")
        rels = rels.replace(
            "</Relationships>",
            '<Relationship Id="rIdHeader1" '
            'Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/header" '
            'Target="header1.xml"/>'
            '<Relationship Id="rIdFooter1" '
            'Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/footer" '
            'Target="footer1.xml"/></Relationships>',
        )
        parts["word/_rels/document.xml.rels"] = rels.encode("utf-8")

        ctypes = parts["[Content_Types].xml"].decode("utf-8")
        if 'Extension="png"' not in ctypes:
            ctypes = ctypes.replace(
                "<Types ",
                "<Types ",
            ).replace(
                "</Types>",
                '<Default Extension="png" ContentType="image/png"/></Types>',
            )
        ctypes = ctypes.replace(
            "</Types>",
            '<Override PartName="/word/header1.xml" '
            'ContentType="application/vnd.openxmlformats-officedocument.'
            'wordprocessingml.header+xml"/>'
            '<Override PartName="/word/footer1.xml" '
            'ContentType="application/vnd.openxmlformats-officedocument.'
            'wordprocessingml.footer+xml"/></Types>',
        )
        parts["[Content_Types].xml"] = ctypes.encode("utf-8")

        OUTPUT.parent.mkdir(parents=True, exist_ok=True)
        with zipfile.ZipFile(OUTPUT, "w", zipfile.ZIP_DEFLATED) as zf:
            # [Content_Types].xml must come first in an OPC package.
            zf.writestr("[Content_Types].xml", parts.pop("[Content_Types].xml"))
            for name, data in parts.items():
                zf.writestr(name, data)

        size_kb = OUTPUT.stat().st_size / 1024
        print(f"\nWrote {OUTPUT.relative_to(PKG)} ({size_kb:.0f} KB)")
    finally:
        shutil.rmtree(work, ignore_errors=True)


if __name__ == "__main__":
    build()
