# What is the Iterator Design Pattern?
#
# - The iterator pattern provides you with a uniform way to access
#   different collections of Object
# - If you get an Array, ArrayList and Hashtable of Object,
#   you pop out an iterator for each and treat them the same
# - This provides a uniform way to cycle through different collections
# - You can also write polymorphic code because you can refer to
#   each collection of objects because they'll implement the same interface
#
# @see http://www.newthinktank.com/2012/10/iterator-design-pattern-tutorial/

# Although we're using Arrays, Sets and Hashes, they all implement Enumerable,
# so we can take the advantage of using the iterator pattern without implementing
# it by hand.
# https://ruby-doc.org/core-2.6.5/Hash.html
# https://ruby-doc.org/stdlib-2.6.5/libdoc/set/rdoc/index.html
# https://ruby-doc.org/core-2.6.5/Hash.html
# https://ruby-doc.org/core-2.6.5/Enumerable.html

require 'set'

class SongInfo
  attr_reader :name, :band, :release_year

  def initialize(name, band, release_year)
    @name = name
    @band = band
    @release_year = release_year
  end
end

class SongsOfThe70s
  attr_reader :best_songs

  def initialize
    @best_songs = []
    add_song('Imagine', 'John Lennon', 1971)
    add_song('American Pie', 'Don McLean', 1971)
    add_song('I Will Survive', 'Gloria Gaynor', 1979)
  end

  def add_song(name, band, release_year)
    song_info = SongInfo.new(name, band, release_year)
    best_songs.push song_info
  end

  def songs
    best_songs
  end
end

class SongsOfThe80s
  attr_reader :best_songs

  def initialize
    @best_songs = Set.new
    add_song('Roam', 'B 52s', 1989)
    add_song('Cruel Summer', 'Bananarama', 1984)
    add_song('Head Over Heels', 'Tears For Fears', 1985)
  end

  def add_song(name, band, release_year)
    song_info = SongInfo.new(name, band, release_year)
    best_songs.add song_info
  end

  def songs
    best_songs
  end
end

class SongsOfThe90s
  attr_reader :best_songs
  attr_accessor :hash_key

  def initialize
    @best_songs = {}
    @hash_key = 0
    add_song('Losing My Religion', 'REM', 1991)
    add_song('Creep', 'Radiohead', 1993)
    add_song('Walk on the Ocean', 'Toad The Wet Sprocket', 1991)
  end

  def add_song(name, band, release_year)
    song_info = SongInfo.new(name, band, release_year)
    best_songs[hash_key] = song_info
    self.hash_key += 1
  end

  def songs
    best_songs.values
  end
end


class DiskJockey
  attr_reader :songs_of_the_70s, :songs_of_the_80s, :songs_of_the_90s

  def initialize(songs_of_the_70s, songs_of_the_80s, songs_of_the_90s)
    @songs_of_the_70s = songs_of_the_70s
    @songs_of_the_80s = songs_of_the_80s
    @songs_of_the_90s = songs_of_the_90s
  end

  def show_the_songs
    puts 'Songs of the 70s'
    show_decade_songs(songs_of_the_70s.songs)
    puts "\nSongs of the 80s"
    show_decade_songs(songs_of_the_80s.songs)
    puts "\nSongs of the 90s"
    show_decade_songs(songs_of_the_90s.songs)
  end

  def show_decade_songs(enumerable_songs)
    enumerable_songs.each do |song|
      puts "Name:         #{song.name}"
      puts "Band:         #{song.band}"
      puts "Release year: #{song.release_year}"
    end
  end
end

################################################################################

songs_70s = SongsOfThe70s.new
songs_80s = SongsOfThe80s.new
songs_90s = SongsOfThe90s.new
mad_mike = DiskJockey.new(songs_70s, songs_80s, songs_90s)
mad_mike.show_the_songs
