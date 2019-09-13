# What is the strategy design pattern?
#
# - When you want to define a class that will have one behavior that is
#   similar to other behaviors in a list
# - When you need to use one of several behaviors dynamically
# - You use this pattern if you need to dynamically change an
#   algorithm used by an object at run time.
# -  Allows you to eliminate code duplication
# - Separates behavior from super and subclasses
#
# @see http://www.newthinktank.com/2012/08/strategy-design-pattern-tutorial/

class Animal
  attr_accessor :flying_type

  def try_to_fly
    flying_type.fly
  end
end

class ItFlys
  def fly
    'flying high'
  end
end

class CantFly
  def fly
    "I can't fly"
  end
end

class Dog < Animal
  def initialize
    @flying_type = CantFly.new
  end
end

class Bird < Animal
  def initialize
    @flying_type = ItFlys.new
  end
end

################################################################################

sparky = Dog.new
tweety = Bird.new

puts "Dog : #{sparky.try_to_fly}"
puts "Bird: #{tweety.try_to_fly}"
puts ''
puts 'Making sparky fly!'
sparky.flying_type = ItFlys.new
puts "Dog: #{sparky.try_to_fly}"
