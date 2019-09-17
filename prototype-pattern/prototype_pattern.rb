=begin
What is the Prototype Pattern?

- Creating new objects by cloning other objects
- Allows for adding of any subclass instance of a known super
  class at run time
- When there are numerous potential classes that you want to
  only use if needed at runtime
- Reduces the need for creating subclasses

@see http://www.newthinktank.com/2012/09/prototype-design-pattern-tutorial/
=end

# in ruby `clone` is available in the Kernel module

class Animal
  def make_copy
    raise NotImplementedError
  end
end

class Sheep < Animal
  def initialize
    puts 'Sheep is made'
  end

  def make_copy
    puts 'sheep is being made'

    clone
  end

  def to_s
    'Dolly is my Hero, Baaaaa'
  end
end

class CloneFactory
  def get_clone(animal_sample)
    animal_sample.make_copy
  end
end

################################################################################

animal_maker = CloneFactory.new
sally = Sheep.new
cloned_sheep = animal_maker.get_clone(sally)

puts sally
puts cloned_sheep

puts "Sally object id: #{sally.object_id}"
puts "Clone object id: #{cloned_sheep.object_id}"
