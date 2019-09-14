=begin
What is the Abstract Factory Pattern?

- It is like a factory, but everything is encapsulated
  - The method that orders the subject
  - The factories that build the object
  - The final objects
  - The final objects contain objects that use Strategy Pattern
    - Composition: Object class fields are objects

@see http://www.newthinktank.com/2012/09/abstract-factory-design-pattern/
=end

class ESUFOGun
  def to_s
    '10 damage'
  end
end

class ESUFOEngine
  def to_s
    '1000 km/h'
  end
end

class ESUFOBossGun
  def to_s
    '40 damage'
  end
end

class ESUFOBossEngine
  def to_s
    '2000 km/h'
  end
end

class EnemyShipBuilding
  def order_the_ship(ship_type)
    make_enemy_ship(ship_type).tap do |new_ship|
      new_ship.make_ship
      new_ship.display_enemy_ship
      new_ship.follow_hero_ship
      new_ship.enemy_ship_shoots
    end
  end
end

class UFOEnemyShipBuilding < EnemyShipBuilding
  def make_enemy_ship(ship_type)
    new_ship = nil

    case ship_type
    when 'UFO'
      ship_parts_factory = UFOEnemyShipFactory.new
      new_ship = UFOEnemyShip.new(ship_parts_factory)
      new_ship.name = 'UFO Grunt Ship'
    when 'UFO BOSS'
      ship_parts_factory = UFOBossEnemyShipFactory.new
      new_ship = UFOBossEnemyShip.new(ship_parts_factory)
      new_ship.name = 'UFO Boss Ship'
    end

    new_ship
  end
end

class UFOEnemyShipFactory
  def add_es_gun
    ESUFOGun.new
  end

  def add_es_engine
    ESUFOEngine.new
  end
end

class UFOBossEnemyShipFactory
  def add_es_gun
    ESUFOBossGun.new
  end

  def add_es_engine
    ESUFOBossEngine.new
  end
end

class EnemyShip
  attr_accessor :name, :weapon, :engine

  def follow_hero_ship
    puts "#{name} is following the hero at #{engine}"
  end

  def display_enemy_ship
    puts "#{name} is on the screen"
  end

  def enemy_ship_shoots
    puts "#{name} attacks and does #{weapon}"
  end

  def to_s
    "The #{name} has a top speed of #{engine} and an attack power of #{weapon}"
  end
end

class UFOEnemyShip < EnemyShip
  attr_reader :ship_factory

  def initialize(ship_factory)
    @ship_factory = ship_factory
  end

  def make_ship
    puts "Making enemy ship #{name}"

    self.weapon = ship_factory.add_es_gun
    self.engine = ship_factory.add_es_engine
  end
end

class UFOBossEnemyShip < EnemyShip
  attr_reader :ship_factory

  def initialize(ship_factory)
    @ship_factory = ship_factory
  end

  def make_ship
    puts 'Making enemy ship ' + name

    self.weapon = ship_factory.add_es_gun
    self.engine = ship_factory.add_es_engine
  end
end

################################################################################

make_ufos = UFOEnemyShipBuilding.new

the_grunt = make_ufos.order_the_ship("UFO")
the_boss = make_ufos.order_the_ship("UFO BOSS")

puts the_grunt
puts the_boss
