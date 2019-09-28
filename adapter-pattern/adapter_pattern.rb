=begin
What is the Adapter Pattern?

- Allows two incomplatible interfaces to work togheter
- Used when the client expects a (target) interface
- The adapter class allows the use of the available interface and
  the Target interface
- Any class can work togheter as long as the Adapter solves the
  issue that all classes must implement every method by the shared interface

@see http://www.newthinktank.com/2012/09/adapter-design-pattern-tutorial/
=end

class EnemyTank
  def fire_weapon
    damage = rand(10) + 1
    puts "Enemy Tank Does #{damage} damage"
  end

  def drive_forward
    movement = rand(5) + 1
    puts "Enemy Tank moves #{movement} spaces"
  end

  def assign_driver(driver_name)
    puts "#{driver_name} is driving the tank"
  end
end

class EnemyRobot
  def smash_with_hands
    damage = rand(10) + 1
    puts "Enemy Robot causes #{damage} damage"
  end

  def walk_forward
    movement = rand(10) + 1
    puts "Enemy Robot walks forward #{movement} spaces"
  end

  def react_to_human(human_name)
    puts "Enemy Robot tramps on #{human_name}"
  end
end

class EnemyRobotAdapter
  def initialize(robot)
    @robot = robot
  end

  def fire_weapon
    @robot.smash_with_hands
  end

  def drive_forward
    @robot.walk_forward
  end

  def assign_driver(driver_name)
    @robot.react_to_human(driver_name)
  end
end

#################################################################################

tank = EnemyTank.new
robot = EnemyRobot.new
robot_adapter = EnemyRobotAdapter.new(robot)

puts 'The Robot'
robot.react_to_human('Joao')
robot.walk_forward
robot.smash_with_hands

puts 'The Enemy Tank'
tank.assign_driver('Joao')
tank.drive_forward
tank.fire_weapon

puts 'The Robot Adapater'
robot_adapter.assign_driver('Joao')
robot_adapter.drive_forward
robot_adapter.fire_weapon
