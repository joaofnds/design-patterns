=begin
What is the Factory Pattern?

- When a method return one of several possible classes that
  share a common super class
Ex:
  - Create a new enemy in a game
  - Random number generator picks a number assigned to a specific enemy
  - The factory returns the enemy associated with that number
- The class is chosen at run time

@see http://www.newthinktank.com/2012/09/factory-design-pattern-tutorial/
=end

class EnemyShip
  attr_accessor :name, :damage

  def follow_hero_ship
    puts name + ' is following the hero'
  end

  def display_enemy_ship
    puts name + ' is on the screen'
  end

  def enemy_ship_shoots
    puts name + ' attacks and does ' + damage.to_s
  end
end

class UFOEnemyShip < EnemyShip
  def initialize
    @name = 'UFO Enemy Ship'
    @damage = 20
  end
end

class RocketEnemyShip < EnemyShip
  def initialize
    @name = 'Rocket Enemy Ship'
    @damage = 10
  end
end

class BigUFOEnemyShip < EnemyShip
  def initialize
    @name = 'Big UFO Enemy Ship'
    @damage = 40
  end
end

class EnemyShipFactory
  attr_reader :ship_type

  def initialize(ship_type)
    @ship_type = ship_type
  end

  def make_ship
    case ship_type
    when 'U'
      UFOEnemyShip.new
    when 'R'
      RocketEnemyShip.new
    when 'B'
      BigUFOEnemyShip.new
    end
  end
end

################################################################################

def exercise_ship(ship)
  ship.follow_hero_ship
  ship.display_enemy_ship
  ship.enemy_ship_shoots
end

puts 'What type ship? (U/R/B) '
ship_type = gets.chomp
new_ship = EnemyShipFactory.new(ship_type).make_ship
if new_ship.nil?
  puts 'invalid ship type'
else
  exercise_ship(new_ship)
end
