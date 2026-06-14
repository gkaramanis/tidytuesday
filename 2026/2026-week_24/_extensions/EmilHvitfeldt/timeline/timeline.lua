function Pandoc(doc)
  if quarto.doc.is_format("html") then
    quarto.doc.add_html_dependency({
      name = 'timeline',
      version = '0.1.0',
      stylesheets = { 'timeline.css' },
      scripts = { 'timeline.js' }
    })
  end
  return doc
end
