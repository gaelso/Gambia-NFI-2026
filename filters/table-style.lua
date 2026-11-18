-- Word (docx) output: alignment and header style of the tables.
--
-- Alignment: Quarto wraps each captioned table in a one-cell table with a centred column, and pandoc applies
-- that alignment to all the paragraphs of the nested table, after their own column alignment, so all the cells
-- end up centred. The wrapper column is set back to the default alignment: the cells keep the alignment of
-- their column (kable: numbers right, text left). Wrappers of figures (no nested table) stay centred.
-- Header: the column names are in bold.

local function has_nested_table(tbl)
  local found = false
  local function check_rows(rows)
    for _, row in ipairs(rows) do
      for _, cell in ipairs(row.cells) do
        pandoc.walk_block(pandoc.Div(cell.contents), { Table = function() found = true end })
      end
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

local function bold_header(tbl)
  for _, row in ipairs(tbl.head.rows) do
    for _, cell in ipairs(row.cells) do
      cell.contents = cell.contents:walk({
        Plain = function(el) return pandoc.Plain({ pandoc.Strong(el.content) }) end,
        Para  = function(el) return pandoc.Para({ pandoc.Strong(el.content) }) end,
      })
    end
  end
end

function Table(tbl)
  if not quarto.doc.is_format("docx") then return nil end

  if has_nested_table(tbl) then
    -- Caption wrapper of a table
    for i, cs in ipairs(tbl.colspecs) do
      tbl.colspecs[i] = { pandoc.AlignDefault, cs[2] }
    end
    for _, body in ipairs(tbl.bodies) do
      for _, row in ipairs(body.body) do
        for _, cell in ipairs(row.cells) do cell.alignment = pandoc.AlignDefault end
      end
    end
    return tbl
  end

  if #tbl.head.rows > 0 then
    bold_header(tbl)
    return tbl
  end
end
