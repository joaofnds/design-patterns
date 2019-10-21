# What is the Bridge Pattern?
#
# - Decouple an abstraction from its implementation so that the
#   two can vary independently
# - The bridge pattern is very poorly explained, everyone seems
#   to explain it differenlty
# - Progessively adding functionality while separating out major
#   differences using abstract class
#
# @see http://www.newthinktank.com/2012/10/bridge-design-pattern-tutorial/

class EntertainmentDevice
  attr_accessor :device_state, :max_setting, :volume_setting

  def button_five_pressed; end

  def button_six_pressed; end

  def device_feedback
    device_state = 0 if device_state > max_setting || device_state < 0
    puts 'On ' + device_state
  end

  def button_seven_pressed
    volume_level += 1
    puts 'Volume at' + volume_level.to_s
  end
end

class TVDevice < EntertainmentDevice
  def initialize(device_state, max_setting)
    @device_state = device_state
    @max_setting = max_setting
  end

  def button_five_pressed
    puts 'Channel Down'
    @device_state -= 1
  end

  def button_six_pressed
    puts 'Channel Up'
    @device_state += 1
  end
end

class RemoteButton
  attr_accessor :the_device

  def initialize(device)
    @the_device = device
  end

  def button_five_pressed
    @the_device.button_five_pressed
  end

  def button_six_pressed
    @the_device.button_six_pressed
  end

  def device_feedback
    @the_device.device_feedback
  end
end

class TVRemoteMute < RemoteButton
  def button_nine_pressed
    puts 'TV was Muted'
  end
end

class TVRemotePause < RemoteButton
  def button_nine_pressed
    puts 'TV was paused'
  end
end

################################################################################

tv = TVRemoteMute.new(TVDevice.new(1, 10))
tv2 = TVRemotePause.new(TVDevice.new(1, 10))

puts 'Test TV with Mute'
tv.button_five_pressed
tv.button_six_pressed
tv.button_nine_pressed

puts

puts 'Test TV with Pause'
tv2.button_five_pressed
tv2.button_six_pressed
tv2.button_nine_pressed
