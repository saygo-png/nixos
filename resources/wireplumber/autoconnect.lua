-- this is mostly chatgpt generated based on https://github.com/bennetthardwick/dotfiles/blob/607bee30e53d735dfda2c89179024527c73e885d/.config/wireplumber/scripts/auto-connect-ports.lua

-- Create a link between two ports
local function create_link(output_port, input_port)
  if not output_port or not input_port then
    return nil
  end

  local link = Link("link-factory", {
    ["link.input.node"] = input_port.properties["node.id"],
    ["link.input.port"] = input_port.properties["object.id"],
    ["link.output.node"] = output_port.properties["node.id"],
    ["link.output.port"] = output_port.properties["object.id"],
    ["object.linger"] = true,
    ["node.description"] = "Auto-connected by script",
  })

  link:activate(1)
  return link
end

-- Filter ports to only those belonging to the node with lowest ID
local function filter_lowest_node_id(object_manager, constraint)
  local all_ports = {}
  local nodes_by_id = {}

  -- Collect all matching ports and group by node ID
  for port in object_manager:iterate(constraint) do
    local node_id = port.properties["node.id"]
    if node_id then
      if not nodes_by_id[node_id] then
        nodes_by_id[node_id] = {}
      end
      table.insert(nodes_by_id[node_id], port)
      table.insert(all_ports, { port = port, node_id = tonumber(node_id) })
    end
  end

  if #all_ports == 0 then
    return {}
  end

  -- Find the lowest node ID
  local lowest_id = math.huge
  for _, entry in pairs(all_ports) do
    if entry.node_id < lowest_id then
      lowest_id = entry.node_id
    end
  end

  -- Return only ports from the lowest ID node
  return nodes_by_id[tostring(lowest_id)] or {}
end

-- Method 1: Connect by port index (first 2 ports, optionally lowest node ID only)
local function auto_connect_by_index(config)
  local output_om = ObjectManager({
    Interest({ type = "port", config.output, Constraint({ "port.direction", "equals", "out" }) }),
  })

  local input_om = ObjectManager({
    Interest({ type = "port", config.input, Constraint({ "port.direction", "equals", "in" }) }),
  })

  local links = {}

  local function connect_by_index()
    -- Clear existing links
    for _, link in pairs(links) do
      link:request_destroy()
    end
    links = {}

    local outputs = config.lowest_output_node and filter_lowest_node_id(output_om) or {}
    local inputs = config.lowest_input_node and filter_lowest_node_id(input_om) or {}

    if not config.lowest_output_node then
      for port in output_om:iterate() do
        table.insert(outputs, port)
      end
    end

    if not config.lowest_input_node then
      for port in input_om:iterate() do
        table.insert(inputs, port)
      end
    end

    -- Connect first N ports
    local max_connections = math.min(#outputs, #inputs, config.max_channels or 2)
    for i = 1, max_connections do
      local link = create_link(outputs[i], inputs[i])
      if link then
        table.insert(links, link)
      end
    end
  end

  output_om:connect("object-added", connect_by_index)
  input_om:connect("object-added", connect_by_index)
  output_om:activate()
  input_om:activate()
end

-- Method 2: Connect all available ports (stereo sink to any stereo device)
local function auto_connect_all_ports(config)
  local output_om = ObjectManager({
    Interest({ type = "port", config.output, Constraint({ "port.direction", "equals", "out" }) }),
  })

  local input_om = ObjectManager({
    Interest({ type = "port", config.input, Constraint({ "port.direction", "equals", "in" }) }),
  })

  local links = {}

  local function connect_all()
    for _, link in pairs(links) do
      link:request_destroy()
    end
    links = {}

    local outputs = config.lowest_output_node and filter_lowest_node_id(output_om) or {}
    local inputs = config.lowest_input_node and filter_lowest_node_id(input_om) or {}

    if not config.lowest_output_node then
      for port in output_om:iterate() do
        table.insert(outputs, port)
      end
    end

    if not config.lowest_input_node then
      for port in input_om:iterate() do
        table.insert(inputs, port)
      end
    end

    if #outputs == 0 then
      return
    end

    for i, input in pairs(inputs) do
      -- Connect each input to corresponding output (wrapping around if needed)
      local output_index = ((i - 1) % #outputs) + 1
      local link = create_link(outputs[output_index], input)
      if link then
        table.insert(links, link)
      end
    end
  end

  output_om:connect("object-added", connect_all)
  input_om:connect("object-added", connect_all)
  output_om:activate()
  input_om:activate()
end

-- Microphone -> Carla processing -> Virtual mic
auto_connect_all_ports({
  output = Constraint({ "port.alias", "matches", "USB PnP Audio Device:*" }),
  input = Constraint({ "port.alias", "matches", "Carla:audio-in*" }),
  lowest_input_node = true,
})

auto_connect_by_index({
  output = Constraint({ "port.alias", "matches", "Carla:*" }),
  input = Constraint({ "port.alias", "matches", "Virtual microphone device:*" }),
  max_channels = 2,
})
