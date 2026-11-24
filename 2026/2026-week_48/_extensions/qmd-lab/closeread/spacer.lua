function spacer(args)
  local spacerLength = "40svh"
  if args[1] ~= nil then
    spacerString = pandoc.utils.stringify(args[1])
  end

  -- split spacer height into number and units
  local _, _, spacerNumStr, spacerUnits =
    string.find(spacerString, "(%d+)%s?(%a+);?")
  local spacerNum = tonumber(spacerNumStr)

  if (spacerNum == nil) then
    error("Spacer height should be a number followed by a CSS unit, not " ..
      spacerString)
  end
  
  -- halve and join back up
  local spacerHeightHalved = tostring(spacerNum / 2) .. (spacerUnits or "")

  return pandoc.Div({},
    pandoc.Attr("",
      { "cr-spacer" },
      { style = "padding-block: " .. spacerHeightHalved .. ";" }))
end
