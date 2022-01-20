/**
 * What is the Command Design Pattern?
 *
 * - The command pattern is a behavioural design pattern in which an object is
 *   used to represent and encapsulate all the information needed to call
 *   a method at a later time
 * - This information includes the method name, the object that owns the method
 *   and values for the method parameters
 * - Allows you to store lists of code that is used at a later time
 *   or many times
 * - Client says "I want a specific Command to run when execute() is called on
 *   one of these encapsulated(hidden) Objects"
 * - An Object called the Invoker transfers this Command to another Object
 *   called a Receiver to execute the right code
 *
 * @see http://www.newthinktank.com/2012/09/command-design-pattern-tutorial/
 */

interface ElectronicDevice {
  on(): void;
  off(): void;
  volumeUp(): void;
  volumeDown(): void;
}

class Television implements ElectronicDevice {
  private volume: number = 0;

  on(): void {
    console.log("TV is ON");
  }

  off(): void {
    console.log("TV is OFF");
  }

  volumeUp(): void {
    this.volume++;
    this.displayVolume();
  }

  volumeDown(): void {
    this.volume--;
    this.displayVolume();
  }

  private displayVolume() {
    console.log(`TV Volume is at ${this.volume}`);
  }
}

interface Command {
  execute(): void;
}

class TurnTVOn implements Command {
  private device: ElectronicDevice;

  public constructor(device: ElectronicDevice) {
    this.device = device;
  }

  execute(): void {
    this.device.on();
  }
}

class TurnTVOff implements Command {
  private device: ElectronicDevice;

  public constructor(device: ElectronicDevice) {
    this.device = device;
  }

  execute(): void {
    this.device.off();
  }
}

class TurnTVUp implements Command {
  private device: ElectronicDevice;

  public constructor(device: ElectronicDevice) {
    this.device = device;
  }

  execute(): void {
    this.device.volumeUp();
  }
}

class TurnTVDown implements Command {
  private device: ElectronicDevice;

  public constructor(device: ElectronicDevice) {
    this.device = device;
  }

  execute(): void {
    this.device.volumeDown();
  }
}

class DeviceButton {
  private command: Command;

  public constructor(command: Command) {
    this.command = command;
  }

  public press(): void {
    this.command.execute();
  }
}

class TVRemote {
  public static getDevice(): ElectronicDevice {
    return new Television();
  }
}

//------------------------------------------------------------------------------

const device = TVRemote.getDevice();

const commands = {
  on: new TurnTVOn(device),
  off: new TurnTVOff(device),
  up: new TurnTVUp(device),
  down: new TurnTVDown(device),
};

const buttons = {
  on: new DeviceButton(commands.on),
  off: new DeviceButton(commands.off),
  up: new DeviceButton(commands.up),
  down: new DeviceButton(commands.down),
};

buttons.off.press();
buttons.on.press();
buttons.up.press();
buttons.down.press();
