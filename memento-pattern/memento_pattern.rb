# What is the Memento Design Pattern?
# - Used to store an object state at a point in time so it can be
#   returned to that state later.
# - It simply allows you to undo/redo changes on an Object
# - Memento: The basic object that is stored in different states
# - Originator: Sets and Gets values from the currently targeted
#   Memento. Create new Mementos and assigns current values to them
# - Caretaker: Holds a List that contains all previous versions of the Memento.
#   It can store and retrieve stored Mementos
#
# @see http://www.newthinktank.com/2012/10/memento-design-pattern-tutorial/

class Memento
  attr_reader :article

  def initialize(article)
    @article = article
  end
end

class Originator
  attr_accessor :article

  def initialize(article)
    @article = article
  end

  def to_memento
    Memento.new(article)
  end

  def from_memento(memento)
    article = memento.article
  end
end

class CareTaker
  attr_reader :versions

  def initialize
    @versions = []
  end

  def commit(memento)
    @versions << memento
  end

  def get(index)
    @versions[index]
  end
end

class Editor
  attr_accessor :text

  def initialize
    @text = ''
    @originator = Originator.new(@text)
    @care_taker = CareTaker.new
    @saved_versions = 0
    @current_version = 0
  end

  def save
    originator.article = @text
    care_taker.commit(originator.to_memento)
    @saved_versions += 1
    @current_version += 1
  end

  def undo
    if @current_version >= 1
      @current_version -= 1
      @text = originator.from_memento(care_taker.get(@current_version))
    end
  end

  def redo
    if (@saved_versions - 1) > @current_version
      @current_version += 1
      @text = originator.from_memento(care_taker.get(@current_version))
    end
  end

  def versions
    care_taker.versions
  end

  private

  attr_reader :originator, :care_taker
end

################################################################################

editor = Editor.new
%w[the quick brown fox jumped over the lazy].each do |word|
  editor.text += "#{word} "
  editor.save
end

# oops, typo
editor.text += "fog"
# undo to forget typo
editor.undo
# oops, passed the point
editor.undo
# no worries, we cand redo too ;)
editor.redo
# add the correct word
editor.text += "dog"
# aaaaaand save it!
editor.save

editor.versions.each do |version|
  puts version.article
end
