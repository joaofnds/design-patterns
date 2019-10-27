# What is the Composite Design Pattern?
# - Allows you to treat individual objects and
#   compositions of objects uniformly
# - They allow you to represent part-whole hierarchies
# - Components can be further divided into smaller components
# - You can structure data or represent the inner working of
#   every part of a whole object individually
#
# @see http://www.newthinktank.com/2012/10/composite-design-pattern-tutorial/

class SongComponent
  def add(_song)
    raise NotImplementedError
  end

  def remove(_song)
    raise NotImplementedError
  end

  def component(_component_index)
    raise NotImplementedError
  end

  def song_name
    raise NotImplementedError
  end

  def band_name
    raise NotImplementedError
  end

  def release_year
    raise NotImplementedError
  end
end

class SongGroup < SongComponent
  attr_reader :components, :name, :description

  def initialize(name, description)
    @components = []
    @name = name
    @description = description
  end

  def add(song)
    components.push(song)
  end

  def remove(song)
    components.delete(song)
  end

  def get(index)
    song_components.at(index)
  end

  def info
    info_str = "#{name} #{description}\n"
    info_str += components.map(&:info).join("\n")
    info_str
  end
end

class Song < SongComponent
  attr_reader :name, :band, :release_year

  def initialize(name, band, release_year)
    @name = name
    @band = band
    @release_year = release_year
  end

  def info
    "#{name} was recorded by #{band} in #{release_year}"
  end
end

################################################################################

industrial = SongGroup.new('Industrial Music', 'is a style of experimental music that draws on transgressive and provocative themes')
heavy_metal = SongGroup.new('Heavy Metal', 'is a genre of rock that developed in the late 1960s, largely in the UK and in the US')
dubstep = SongGroup.new('DubStep', 'is a genre of electronic dance music that originated in South London, England')
every_song = SongGroup.new('Song List', 'Every Song Available')

industrial.add(dubstep)

industrial.add(Song.new('Head Like a Hole', 'NIN', 1990))
industrial.add(Song.new('Headhunter', 'Front 242', 1988))

dubstep.add(Song.new('Centipede', 'Knife Party', 2012))
dubstep.add(Song.new('Tetris', 'Doctor P', 2011))

heavy_metal.add(Song.new('War Pigs', 'Black Sabath', 1970))
heavy_metal.add(Song.new('Ace of Spades', 'Motorhead', 1980))

every_song.add(industrial)
every_song.add(heavy_metal)

puts every_song.info
