/**
 * What is the Bridge Pattern?
 *
 * - Decouple an abstraction from its implementation so that the
 *   two can vary independently
 * - The bridge pattern is very poorly explained, everyone seems
 *   to explain it differenlty
 * - Progessively adding functionality while separating out major
 *   differences using abstract class
 *
 * @see http://www.newthinktank.com/2012/10/bridge-design-pattern-tutorial/
 */

abstract class EntertainmentDevice {
  protected volume: number = 0;

  constructor(protected setting: number, protected maxSetting: number) {}

  abstract pressButtonFive(): void;
  abstract pressButtonSix(): void;

  feedback() {
    if (this.setting > this.maxSetting || this.setting < 0) {
      this.setting = 0;
      console.log(`On: ${this.setting}`);
    }
  }

  pressButtonSeven() {
    this.volume++;
    console.log(`Volume at: ${this.volume}`);
  }

  pressButtonEight() {
    this.volume--;
    console.log(`Volume at: ${this.volume}`);
  }
}

class TVDevice extends EntertainmentDevice {
  pressButtonFive() {
    console.log("Channel Down");
    this.setting--;
  }

  pressButtonSix() {
    console.log("Channel Up");
    this.setting++;
  }
}

abstract class RemoteButton {
  constructor(private device: EntertainmentDevice) {}

  pressButtonFive() {
    this.device.pressButtonFive();
  }

  pressButtonSix() {
    this.device.pressButtonSix();
  }

  abstract pressButtonNine(): void;
}

class TVRemoteMute extends RemoteButton {
  pressButtonNine() {
    console.log("TV muted");
  }
}

class TVRemotePause extends RemoteButton {
  pressButtonNine() {
    console.log("TV paused");
  }
}

//----------------------------------------------------------------------

const device = new TVDevice(1, 200)
const TV: RemoteButton = new TVRemoteMute(device);
const TV2: RemoteButton = new TVRemotePause(device);

TV.pressButtonFive();
TV.pressButtonSix();
TV.pressButtonNine();

TV2.pressButtonNine();
