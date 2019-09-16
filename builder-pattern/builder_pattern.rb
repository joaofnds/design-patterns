=begin
What is the Builder Pattern?

- Pattern used to create objects made from a bunch of other objects
  - When you want to build an object made up from other objects
  - When you want the creation of these parts to be independent
    of the main object
  - Hide the creation of the parts from the client so both
    aren't dependent
  - The builder know the specifics and nobody else does

@see http://www.newthinktank.com/2012/09/builder-design-pattern-tutorial/
=end

class Robot
  attr_accessor :head, :torso, :arms, :legs
end

class OldRobotBuilder
  attr_reader :robot

  def initialize
    @robot = Robot.new
  end

  def build_head
    robot.head = 'Tin Head'
  end

  def build_torso
    robot.torso = 'Tin Torso'
  end

  def build_arms
    robot.arms = 'Blowtorch Arms'
  end

  def build_legs
    robot.legs = 'Roller Skates'
  end
end

class RobotEngineer
  attr_reader :robot_builder

  def initialize(robot_builder)
    @robot_builder = robot_builder
  end

  def robot
    robot_builder.robot
  end

  def make_robot
    robot_builder.build_head
    robot_builder.build_torso
    robot_builder.build_arms
    robot_builder.build_legs
  end
end

################################################################################

old_style_robot = OldRobotBuilder.new
robot_engineer = RobotEngineer.new(old_style_robot)

robot_engineer.make_robot
robot = robot_engineer.robot

puts "robot head:  #{robot.head}"
puts "robot torso: #{robot.torso}"
puts "robot arms:  #{robot.arms}"
puts "robot legs:  #{robot.legs}"
