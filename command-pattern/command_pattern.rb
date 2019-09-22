# What is the Command Design Pattern?
#
# - The command pattern is a behavioural design pattern in which an object is
#   used to represent and encapsulate all the information needed to call
#   a method at a later time
# - This information includes the method name, the object that owns the method
#   and values for the method parameters
# - Allows you to store lists of code that is used at a later time
#   or many times
# - Client says "I want a specific Command to run when execute() is called on
#   one of these encapsulated(hidden) Objects"
# - An Object called the Invoker transfers this Command to another Object
#   called a Receiver to execute the right code
#
# @see http://www.newthinktank.com/2012/09/command-design-pattern-tutorial/

class Television
  attr_reader :state, :volume

  def initialize
    @state = "off"
    @volume = 0
  end

  def on
    self.state = "on"
    show_state
  end

  def off
    self.state = "off"
    show_state
  end

  def volume_up
    self.volume += 1
    show_volume
  end

  def volume_down
    self.volume -= 1
    show_volume
  end

  private

  attr_writer :state, :volume

  def show_state
    puts "TV is " + self.state.upcase
  end

  def show_volume
    puts "TV volume is at " + self.volume.to_s
  end
end

class Radio
  attr_reader :state, :volume

  def initialize
    @state = "off"
    @volume = 0
  end

  def on
    self.state = "on"
    show_state
  end

  def off
    self.state = "off"
    show_state
  end

  def volume_up
    self.volume += 1
    show_volume
  end

  def volume_down
    self.volume -= 1
    show_volume
  end

  private

  attr_writer :state, :volume

  def show_state
    puts "Radio is " + self.state.upcase
  end

  def show_volume
    puts "Radio volume is at " + self.volume.to_s
  end
end

class TurnTVOn
  attr_reader :electronic_device

  def initialize(device)
    @electronic_device = device
  end

  def execute
    electronic_device.on
  end

  def undo
    electronic_device.off
  end
end

class TurnTVOff
  attr_reader :electronic_device

  def initialize(device)
    @electronic_device = device
  end

  def execute
    electronic_device.off
  end

  def undo
    electronic_device.on
  end
end

class TurnTVVolumeUp
  attr_reader :electronic_device

  def initialize(device)
    @electronic_device = device
  end

  def execute
    electronic_device.volume_up
  end

  def undo
    electronic_device.volume_down
  end
end

class TurnTVVolumeDown
  attr_reader :electronic_device

  def initialize(device)
    @electronic_device = device
  end

  def execute
    electronic_device.volume_down
  end

  def undo
    electronic_device.volume_up
  end
end

class DeviceButton
  attr_reader :command

  def initialize(command)
    @command = command
  end

  def press
    command.execute
  end

  def press_undo
    command.undo
  end
end

class TVRemote
  def self.get_device
    Television.new
  end
end

class TurnItAllOff
  attr_reader :devices

  def initialize(devices)
    @devices = devices
  end

  def execute
    devices.each(&:off)
  end

  def undo
    devices.each(&:on)
  end
end

################################################################################

tv = TVRemote.get_device

on_command = TurnTVOn.new(tv)
on_button = DeviceButton.new(on_command)

off_command = TurnTVOff.new(tv)
off_button = DeviceButton.new(off_command)

volume_up_command = TurnTVVolumeUp.new(tv)
volume_up_button = DeviceButton.new(volume_up_command)

volume_down_command = TurnTVVolumeDown.new(tv)
volume_down_button = DeviceButton.new(volume_down_command)

off_button.press
on_button.press
volume_up_button.press
volume_up_button.press
volume_up_button.press
volume_down_button.press

###

the_tv = Television.new
radio = Radio.new

all_off_command = TurnItAllOff.new([tv, the_tv, radio])
all_off_button = DeviceButton.new(all_off_command)

all_off_button.press
all_off_button.press_undo
