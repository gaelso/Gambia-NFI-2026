-- Word (docx) output: Quarto writes all float captions with the 'Image Caption' style.
-- Table captions get the 'Table Caption' style instead, so that the list of tables (lot)
-- finds them and the list of figures (lof) only lists figures.

function Para(el)
  local first = el.content[1]
  if first and first.t == "RawInline" and first.format == "openxml"
    and first.text:find("ImageCaption", 1, true)
    and pandoc.utils.stringify(el):match("^Table") then
    first.text = first.text:gsub("ImageCaption", "TableCaption")
    return el
  end
end
