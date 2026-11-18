-- Runs after Quarto. Uses Lua bundled with Quarto; no Python is required.
if FORMAT ~= 'docx' then return {} end
local portrait = '<w:p><w:pPr><w:sectPr><w:footerReference r:id="rId9" w:type="default"/><w:type w:val="nextPage"/><w:pgSz w:w="11906" w:h="16838"/><w:pgMar w:top="1440" w:right="1440" w:bottom="1440" w:left="1440" w:header="720" w:footer="720" w:gutter="0"/></w:sectPr></w:pPr></w:p>'
return {
  {Table=function(t)
    if #t.colspecs==1 and #t.head.rows==0 and #t.bodies==1 and #t.bodies[1].body==1 then
      local cells=t.bodies[1].body[1].cells
      if #cells==1 and #cells[1].contents==1 and cells[1].contents[1].t=='Div' then
        local d=cells[1].contents[1]
        if d.identifier:match('^tbl%-') or d.identifier:match('^fig%-') then return d end
      end
    end
  end},
  {Div=function(d)
    local tbl=d.identifier:match('^tbl%-')
    local fig=d.identifier:match('^fig%-')
    if tbl or fig then
      for i,b in ipairs(d.content) do
        if fig and b.t=='Plain' then
          d.content[i]=pandoc.Div({pandoc.Para(b.content)},pandoc.Attr('',{}, {['custom-style']='Figure Image'}))
        end
        if b.t=='Para' and b.content[1] and b.content[1].t=='RawInline' and b.content[1].text:find('ImageCaption') then
          b.content:remove(1)
          d.content[i]=pandoc.Div({b},pandoc.Attr('',{}, {['custom-style']=tbl and 'Table Caption' or 'Image Caption'}))
        end
      end
      return d
    end
  end,
  Table=function(t)
    local matrix=pandoc.utils.stringify(t):find('Key analysis outputs')
    t.attributes['custom-style']=matrix and 'MatrixTable' or 'ReportTable'
    for _,row in ipairs(t.head.rows) do
      for _,cell in ipairs(row.cells) do
        for _,b in ipairs(cell.contents) do
          if b.t=='Plain' or b.t=='Para' then b.content={pandoc.Strong(b.content)} end
        end
      end
    end
    return t
  end,
  Header=function(h)
    if h.identifier=='sec-nfi-results-framework-matrix' then
      return {pandoc.RawBlock('openxml',portrait),h}
    end
  end}
}
