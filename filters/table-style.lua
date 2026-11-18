-- Word (docx) output: layout and text style of the data tables.
--
-- Column widths: kable pipe tables have long lines, so pandoc gives them relative column widths from the dash
-- counts of the separator line and writes them with a fixed layout, where the narrow numeric columns overflow.
-- The widths are reset to default: Word autofits the columns to their content within the page width.
-- Text style: the cell content gets the 'Table Text' paragraph style (Arial 9, see 'report/reference.docx').
-- (pandoc writes Plain cell content with the 'Compact' style, so Plain blocks become Para blocks.)
-- Only data tables are changed: tables with a header row and no nested table. Quarto's one-cell wrapper tables
-- around captioned tables and figures are left as they are.

local function has_nested_table(tbl)
  local found = false
  local function check(blocks)
    pandoc.walk_block(pandoc.Div(blocks), { Table = function() found = true end })
  end
  local function check_rows(rows)
    for _, row in ipairs(rows) do
      for _, cell in ipairs(row.cells) do check(cell.contents) end
    end
  end
  check_rows(tbl.head.rows)
  for _, body in ipairs(tbl.bodies) do
    check_rows(body.head)
    check_rows(body.body)
  end
  check_rows(tbl.foot.rows)
  return found
end

local function style_rows(rows)
  for _, row in ipairs(rows) do
    for _, cell in ipairs(row.cells) do
      local blocks = cell.contents:walk({ Plain = function(el) return pandoc.Para(el.content) end })
      cell.contents = { pandoc.Div(blocks, pandoc.Attr("", {}, { ["custom-style"] = "Table Text" })) }
    end
  end
end

function Table(tbl)
  if not quarto.doc.is_format("docx") then return nil end
  if #tbl.head.rows == 0 or has_nested_table(tbl) then return nil end

  for i, cs in ipairs(tbl.colspecs) do
    tbl.colspecs[i] = { cs[1], pandoc.ColWidthDefault }
  end

  style_rows(tbl.head.rows)
  for _, body in ipairs(tbl.bodies) do
    style_rows(body.head)
    style_rows(body.body)
  end
  style_rows(tbl.foot.rows)

  return tbl
end
