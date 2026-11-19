-- Word (docx) output: style and alignment of the float captions.
-- Quarto writes all float captions with the 'Image Caption' style and a left alignment.
-- - Table captions get the 'Table Caption' style instead, so that the list of tables (lot)
--   finds them and the list of figures (lof) only lists figures.
-- - All captions are centred, as the tables and figures (left alignment written by Quarto removed).

function Para(el)
  local first = el.content[1]
  if first and first.t == "RawInline" and first.format == "openxml"
    and first.text:find("ImageCaption", 1, true) then
    if pandoc.utils.stringify(el):match("^Table") then
      first.text = first.text:gsub("ImageCaption", "TableCaption")
    end
    first.text = first.text:gsub('<w:jc w:val="left"%s*/>', '<w:jc w:val="center"/>')
    return el
  end
end
