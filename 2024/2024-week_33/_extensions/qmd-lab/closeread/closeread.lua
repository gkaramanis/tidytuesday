--===============--
-- Closeread.lua --
--===============--
-- This script creates the functions/filters that are used to process 
-- a closeread document into the initial HTML file that is loaded by
-- the browser. The filters are actually run at the very bottom of the
-- script, so to understand the script it might be easiest to start there.

-- set defaults
local debug_mode = false
local trigger_selectors = {["focus-on"] = true}
local cr_attributes = {["pan-to"] = true, 
                      ["scale-by"] = true, 
                      ["highlight-spans"] = true}
local remove_header_space = false
local global_layout = "sidebar-left"


--======================--
-- Process YAML options --
--======================--

function read_meta(m)
  
  -- debug mode
  if m["debug-mode"] ~= nil then
    debug_mode = m["debug-mode"]
  end

  -- remove-header-space
  if m["remove-header-space"] ~= nil then
    remove_header_space = m["remove-header-space"]
  end

  -- layout options
  if m["cr-section"] ~= nil then
    if m["cr-section"]["layout"] ~= nil then
      global_layout = m["cr-section"]["layout"][1].text
    end
  end
  
  -- inject debug mode option in html <meta>
  quarto.doc.include_text("in-header", "<meta cr-debug-mode='" ..
    tostring(debug_mode) .. "'>")

  -- inject remove_header_space options into html <meta>
  quarto.doc.include_text("in-header", "<meta cr-remove-header-space='" ..
    tostring(remove_header_space) .. "'>")
  
end


--=====================--
-- Form CR-Section AST --
--=====================--

-- Construct cr-section AST
function make_section_layout(div)
  
  if div.classes:includes("cr-section") then
    
    -- make key components of cr-section
    narrative_col = make_narrative_col(div.content)
    sticky_col    = make_sticky_col(div.content)
    
    -- identify section layout
    local section_layout = global_layout -- inherit from doc yaml
    for attr, value in pairs(div.attr.attributes) do
      if attr == "layout" then
        section_layout = value -- but override with section attr
      end
    end
    
    -- todo: identify quarto layout to use in section
    --local quarto_layouts = {"column-body", "column-outset", "column-page", 
     -- "column-page-inset", "column-screen-inset", "column-margin"}
    local quarto_layout = "column-screen" -- default
    --if list_includes_any(div.classes, quarto_layouts) then
    --  quarto_layout = ""
    --end

    -- Construct cr-section class list
    local section_class_list = {table.unpack(div.classes)}
    table.insert(section_class_list, quarto_layout)
    table.insert(section_class_list, section_layout)
    
    -- piece together the cr-section
    cr_section = pandoc.Div({narrative_col, sticky_col},
      pandoc.Attr("", section_class_list, {}))

    return cr_section
  end
end


function make_sticky_col(cr_section_blocks)
  
  sticky_blocks = cr_section_blocks:walk {
    traverse = 'topdown',
    Block = function(block)
      if is_sticky(block) then
        block = shift_id_to_block(block)
        block.classes:insert("sticky") 
        return block, false -- if a sticky element is found, don't process child blocks
      else
        return {}
      end
    end
  }

  sticky_col_stack = pandoc.Div(sticky_blocks,
    pandoc.Attr("", {"sticky-col-stack"}))
  sticky_col = pandoc.Div(sticky_col_stack,
    pandoc.Attr("", {"sticky-col"}, {}))
  
  return sticky_col
end


function make_narrative_col(cr_section_blocks)
  
  narrative_blocks = make_narrative_blocks(cr_section_blocks)
  narrative_col = pandoc.Div(pandoc.Blocks(narrative_blocks),
    pandoc.Attr("", {"narrative-col"}, {}))
  
  return narrative_col
end


function make_narrative_blocks(cr_section_blocks)

  local narrative_blocks = {}
  -- iterate over top-level blocks
  for _,block in ipairs(cr_section_blocks) do
    if not is_sticky(block) then
      -- if it's progress-block...
      if block.attr ~= nil then
        if block.attr.classes ~= nil then
          if block.classes:includes("progress-block") then
            -- re-run this function on child blocks
            nested_narr_blocks = make_narrative_blocks(block.content)
            progress_blocks = pandoc.Div(nested_narr_blocks, 
              pandoc.Attr("", {"progress-block"}, {}))
            table.insert(narrative_blocks, progress_blocks) 
            goto endofloop
          end
        end
      end
      
      -- if it's a new trigger
      if is_new_trigger(block) then
        table.insert(block.attr.classes, "narrative")
        local new_trigger_block = wrap_block(block, {"trigger", "new-trigger"})
        table.insert(narrative_blocks, new_trigger_block)
      
      --if it's not a new trigger
      else
        -- if the block can hold attributes, make it a narrative block
        if block.attr ~= nil then
          table.insert(block.attr.classes, "narrative")
        else
          -- if it can't (like a Para), wrap it in a Div that can
          block = wrap_block(block, {"narrative"})
        end
        local not_new_trigger_block = wrap_block(block, {"trigger"})
        table.insert(narrative_blocks, not_new_trigger_block)
      end
    end
    
    ::endofloop::
  end
  
  return pandoc.Blocks(narrative_blocks)
end



function shift_id_to_block(block)

  -- if block contains inlines...
  if pandoc.utils.type(block.content) == "Inlines" then
    -- ... iterate over the inlines...
    for i, inline in pairs(block.content) do
      if inline.identifier ~= nil then
        -- ... to find a "cr-" identifier on the child inline
        if string.match(inline.identifier, "^cr-") then
          -- remove id from the child inline
          local id_to_move = inline.identifier
          block.content[i].attr.identifier = ""
          -- and wrap block in Div with #cr- (and converts Para to Plain)
          block = pandoc.Div(block.content, pandoc.Attr(id_to_move, {}, {}))
        end
      end
    end
  end
            
  return block
end


-- wrap_block: wrap block in a div, adds the classList, and transfers the attributes
function wrap_block(block, classList)
  
  -- extract attributes
  local attributesToMove = {}
  if block.attr ~= nil then
    if block.attributes ~= nil then
      for attr, value in pairs(block.attr.attributes) do
       -- if trigger_selectors[attr] or cr_attributes[attr] then
        attributesToMove[attr] = value
        block.attributes[attr] = nil
        --end
      end
    end
  end
  
  -- construct a pandoc.div with the new attributes to return
  return pandoc.Div(block, pandoc.Attr("", classList, pandoc.AttributeList(attributesToMove)))
end


function is_sticky(block)

  sticky_block_id = false
  sticky_inline_id = false
  
  if block.identifier ~= nil then
    if string.match(block.identifier, "^cr-") then
      sticky_block_id = true
    end
  end
  
  if pandoc.utils.type(block.content) == "Inlines" then
    for _, inline in pairs(block.content) do
      if inline.identifier ~= nil then
        if string.match(inline.identifier, "^cr-") then
          sticky_inline_id = true
        end
      end
    end
  end

  return sticky_block_id or sticky_inline_id
end


function is_new_trigger(block)
  -- it can't be a trigger without attributes
  if block.attributes == nil then
    return false
  end
  
  -- if it has attributes, they must match a selector
  local is_trigger = false
  for selector, _ in pairs(trigger_selectors) do
    if block.attributes[selector] then
      is_trigger = true
      break
    end
  end
  
  return is_trigger
end


function find_in_arr(arr, value)
    for i, v in pairs(arr) do
        if v == value then
            return i
        end
    end
end

-- check whether a class list includes any of the items (also a class or list of classes)
function list_includes_any(class_list, items)
  for _, item in ipairs(items) do
    if class_list:includes(item) then
      return true
    end
  end
  return false
end



--======================--
-- Lineblock Processing --
--======================--

-- Append attributes to any cr line blocks
function add_attributes(lineblock)
  local first_line = lineblock.content[1]
  
  if first_line[1].t == "Str" and first_line[1].text:sub(1,1) == "{" then
    local id = extractIds(first_line)[1]
    local classes = extractClasses(first_line)
    local attr_tab = extractAttr(first_line)
    
    table.remove(lineblock.content, 1)
    lineblock = pandoc.Div(lineblock, pandoc.Attr(id, classes, attr_tab))
  end
  
  return lineblock
end


function extractAttr(el)
  local attr_tab = {}
  local keys_tab = {}
  local vals_tab = {}
  local key_inds = {}
  local ind = 0
  
  -- gather keys and their index
  for _,v in ipairs(el) do
    ind = ind + 1
    if v.t == "Str" then
      v.text = v.text:gsub("[{}]", "")
      if v.text:sub(-1) == "=" then
        table.insert(keys_tab, v.text:sub(1, -2))
        table.insert(key_inds, ind)
      end
    end
  end
  
  -- gather values from index + 1
  for _,v in ipairs(key_inds) do
    if el[v + 1].t == "Quoted" then
      table.insert(vals_tab, el[v + 1].content[1].text)
    else
      table.insert(vals_tab, "")
    end
  end
  
  -- zip them together
  for i = 1, #keys_tab do
    attr_tab[keys_tab[i]] = vals_tab[i]
  end
  
  return attr_tab
end


function extractIds(el)
  local ids = {}
  for _,v in ipairs(el) do
    if v.t == "Str" then
      v.text = v.text:gsub("[{}]", "")
      if v.text:sub(1, 1) == "#" then
        table.insert(ids, v.text:sub(2))
      end
    end
  end
  
  return ids
end


function extractClasses(el)
  local classes = {}
  for _,v in ipairs(el) do
    if v.t == "Str" then
      if v.text:sub(1, 1) == "." then
        table.insert(classes, v.text:sub(2))
      end
    end
  end
  return classes
end

-- Wraps each line of a lineblock in a span with a particular id so that 
-- the js can add highlight classes to particular lines
lineblock_ind = 0
function add_spans(lineblock)
  lineblock_ind = lineblock_ind + 1
  
  new_lb_content = pandoc.List({})
  line_ind = 0
  for _,inlines in ipairs(lineblock.content) do
    line_ind = line_ind + 1
    span_id = "lb" .. tostring(lineblock_ind) .. "-" .. tostring(line_ind)
    double_wrapped_line = pandoc.Inlines(pandoc.Span(inlines, pandoc.Attr(span_id, {}, {})))
    table.insert(new_lb_content, double_wrapped_line)
  end
  
  lineblock.content = new_lb_content

  return lineblock
end



--===================--
-- Focus-on Shortcut --
--===================--

-- Allow a short cut syntax where the presence of something like `@cr-map` in a
-- Para will result it getting wrapped in a focus Div (that is later turned into 
-- a narrative trigger) and the citation is removed
function process_trigger_shortcut(para)
  
  local new_inlines = pandoc.Inlines({})
  local sticky_id = nil
  local focus_attributes = {}
  local skip_next = false
  local prefix = "cr-"
  
  for i, elem in ipairs(para.content) do
    
    -- if there is a Space that follows a Cite, remove it
    if skip_next then
      if elem.t == "Space" then
        skip_next = false -- reset flag and skip this space
      --elseif elem.t ~= "Span" then
      --  skip_next = false -- reset flag but don't skip this element
      --  new_inlines:insert(elem) -- it's not a space or span, so we insert it
      end
    else
      -- process Cite block
      if elem.t == 'Cite' then
        local cite_id = elem.citations[1].id
        -- if it's a cr-cite, don't insert it
        if string.find(cite_id, "^" .. prefix) == 1 then
          -- but grab id for focus block
          sticky_id = cite_id
          -- and remove any Space that precedes it
          if i > 1 and para.content[i-1].t == "Space" then
            new_inlines:remove(#new_inlines)
          end
          skip_next = true -- and prepare to to skip following Space
        else
          -- if it's a cite that's not cr-, insert it
          new_inlines:insert(elem)
        end
      
      -- process Cite within a Span
      elseif elem.t == "Span" and elem.content[1] ~= nil and elem.content[1].t == "Cite" then
        local cite_id = elem.content[1].citations[1].id
        -- if it's a cr-cite, don't insert it
        if string.find(cite_id, "^" .. prefix) == 1 then
          -- but do grab id and attributes for focus block
          sticky_id = cite_id
          for k,v in pairs(elem.attr.attributes) do
            focus_attributes[k] = v
          end
          -- and remove any space that precedes it
          if i > 1 and para.content[i-1].t == "Space" then
            new_inlines:remove(#new_inlines)
          end
          skip_next = true -- and prepare to skip the following Space
        else
        -- if it's a Cite that's not cr-, insert it
          new_inlines:insert(elem)
        end
      else
      -- if it's a neither a Cite nor a Span > Cite, add it
        new_inlines:insert(elem)
      end
    end
  end
  
  para.content = new_inlines
  
  -- if a cr-cite was found, wrap in focus block
  if sticky_id ~= nil then
    local attr = pandoc.Attr("", {}, {['focus-on'] = sticky_id})
    for k, v in pairs(focus_attributes) do
      attr.attributes[k] = v
    end
    wrapped_para = pandoc.Div(para, attr)
    para = wrapped_para
  end
  
  return para
end



--================--
-- HTML Injection --
--================--

quarto.doc.add_html_dependency({
  name = "intersection-observer-polyfill",
  version = "1.0.0",
  scripts = {"intersection-observer.js"}
})
quarto.doc.add_html_dependency({
  name = "scrollama",
  version = "3.2.0",
  scripts = {"scrollama.min.js"}
})
quarto.doc.add_html_dependency({
  name = "closeread",
  version = "0.1.0",
  scripts = {"closeread.js"}
})


--=============--
-- Run Filters --
--=============--

return {
  {
    Meta = read_meta
  },
  {
    LineBlock = add_attributes
  },
  {
    LineBlock = add_spans
  },
  {
    Para = process_trigger_shortcut
  },
  {
    Div = make_section_layout,
    Pandoc = add_classes_to_body
  }
}
