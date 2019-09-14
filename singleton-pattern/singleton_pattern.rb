=begin
What is the Singleton Pattern?

It is used when you want to eliminate the option of instantiating
more than one object

@see http://www.newthinktank.com/2012/09/singleton-design-pattern-tutorial/
=end

# Creating a Singleton class is not really needed in Ruby because
# it's already in the standard library
#
# @see https://docs.ruby-lang.org/en/master/Singleton.html
class Singleton
  private_class_method :new, :allocate

  @instance = nil

  def self.instance
    @instance ||= new
  end
end

################################################################################

begin
  Singleton.new
rescue NoMethodError => e
  puts e
  puts
end

first = Singleton.instance
puts "first = Singleton.instance #=> #{first}"

second = Singleton.instance
puts "second = Singleton.instance #=> #{second}"

puts "first == second #=> #{first == second}"
