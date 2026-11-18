-- Only layout: the source text and methodological review choices stay intact.
function Table(t)
  local n=#t.colspecs
  if n==2 then t.colspecs={{pandoc.AlignLeft,0.31},{pandoc.AlignLeft,0.69}}
  elseif n==3 then t.colspecs={{pandoc.AlignLeft,0.20},{pandoc.AlignLeft,0.30},{pandoc.AlignLeft,0.50}}
  elseif n==5 and not pandoc.utils.stringify(t):find("Key analysis outputs") then t.colspecs={{pandoc.AlignLeft,0.16},{pandoc.AlignLeft,0.12},{pandoc.AlignLeft,0.23},{pandoc.AlignLeft,0.19},{pandoc.AlignLeft,0.30}}
  elseif n==5 then t.colspecs={{pandoc.AlignLeft,0.14},{pandoc.AlignLeft,0.14},{pandoc.AlignLeft,0.20},{pandoc.AlignLeft,0.36},{pandoc.AlignLeft,0.16}} end
  return t
end
