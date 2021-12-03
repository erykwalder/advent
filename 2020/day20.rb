def day20
  tiles = read_tiles
  edge_map = count_edges(tiles)
  tiles.select do |tile|
    edges(tile).count {|edge| edge_map[edge] == 2} == 2
  end.map {|tile| tile[:id]}.reduce(1, :*)
end

def count_edges(tiles)
  map = Hash.new(0)
  tiles.each do |tile|
    edges(tile).flat_map {|edge| [edge, edge.reverse]}.each do |edge|
      map[edge] += 1
    end
  end
  map
end

def edges(tile)
  [
    tile[:rows].first,
    tile[:rows].last,
    tile[:rows].map {|r| r[0]}.join,
    tile[:rows].map {|r| r[-1]}.join
  ]
end

def read_tiles
  File.read('input20.txt').split("\n\n").filter_map do |tile|
    lines = tile.split("\n").reject(&:empty?)
    id = /Tile (\d+):\n/.match(tile).captures[0]
    {id: id.to_i, rows: lines[1..]}
  end
end

p day20