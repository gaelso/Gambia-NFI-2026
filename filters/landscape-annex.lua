-- Landscape annex for Word output.
--
-- The content of the div with class 'landscape-annex' is laid out on A4 landscape pages, the content before it on
-- A4 portrait pages. Word stores the page layout of each section in the last paragraph of the section, and the
-- layout of the last section comes from the reference document ('report/reference.docx': A4 landscape, 2 cm
-- margins). The filter only adds an A4 portrait section break before the div, so the div must be the last
-- block of the document (an empty last section would add a blank page).
-- Quarto's own '.landscape' div is not used: its portrait section break has no page size (Word default, not A4)
-- and it adds an empty last section (blank page) when the div ends the document.

local portrait_break = pandoc.RawBlock("openxml",
  '<w:p><w:pPr><w:sectPr>' ..
  '<w:pgSz w:w="11906" w:h="16838"/>' ..
  '<w:pgMar w:top="1134" w:right="1134" w:bottom="1134" w:left="1134" w:header="567" w:footer="567" w:gutter="0"/>' ..
  '</w:sectPr></w:pPr></w:p>'
)

function Pandoc(doc)
  if not quarto.doc.is_format("docx") then return nil end

  local idx = nil
  for i, blk in ipairs(doc.blocks) do
    if blk.t == "Div" and blk.classes:includes("landscape-annex") then
      idx = i
      break
    end
  end

  if idx == nil then
    -- No landscape part: whole document in portrait, the empty last section adds a blank landscape page
    quarto.log.warning("landscape-annex.lua: no '.landscape-annex' div found, a blank landscape page is added at the end")
    doc.blocks:insert(portrait_break)
  else
    -- Content after the div, ignoring empty divs (Quarto adds an empty hidden div at the end of the document)
    local trailing = false
    for j = idx + 1, #doc.blocks do
      local blk = doc.blocks[j]
      if not (blk.t == "Div" and #blk.content == 0) then trailing = true end
    end
    if trailing then
      quarto.log.warning("landscape-annex.lua: content after the '.landscape-annex' div is also in landscape")
    end
    doc.blocks:insert(idx, portrait_break)
  end

  return doc
end
