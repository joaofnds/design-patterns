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
  protected deviceState: number;
  protected maxSetting: number;
  protected volumeLevel: number = 0;

  public abstract buttonFivePressed(): void;
  public abstract buttonSixPressed(): void;

  public deviceFeedback(): void {
    if (this.deviceState > this.maxSetting || this.deviceState < 0) {
      this.deviceState = 0;
      console.log(`On: ${this.deviceFeedback}`);
    }
  }

  public buttonSevenPressed(): void {
    this.volumeLevel++;
    console.log(`Volume at: ${this.volumeLevel}`);
  }

  public buttonEightPressed(): void {
    this.volumeLevel--;
    console.log(`Volume at: ${this.volumeLevel}`);
  }
}

class TVDevice extends EntertainmentDevice {
  public constructor(newDeviceState: number, newMaxSetting: number) {
    super();
    this.deviceState = newDeviceState;
    this.maxSetting = newMaxSetting;
  }
  public buttonFivePressed(): void {
    console.log("Channel Down");
    this.deviceState--;
  }
  public buttonSixPressed(): void {
    console.log("Channel Up");
    this.deviceState++;
  }
}

abstract class RemoteButton {
  private theDevice: EntertainmentDevice;

  public constructor(newDevice: EntertainmentDevice) {
    this.theDevice = newDevice;
  }

  public buttonFivePressed(): void {
    this.theDevice.buttonFivePressed();
  }

  public buttonSixPressed(): void {
    this.theDevice.buttonSixPressed();
  }

  public abstract buttonNinePressed();
}

class TVRemoveMute extends RemoteButton {
  public constructor(newDevice: EntertainmentDevice) {
    super(newDevice);
  }

  public buttonNinePressed() {
    console.log("TV Muted");
  }
}

class TVRemovePause extends RemoteButton {
  public constructor(newDevice: EntertainmentDevice) {
    super(newDevice);
  }

  public buttonNinePressed() {
    console.log("TV paused");
  }
}

//----------------------------------------------------------------------
const theTV: RemoteButton = new TVRemoveMute(new TVDevice(1, 200));
const theTV2: RemoteButton = new TVRemovePause(new TVDevice(1, 200));
theTV.buttonFivePressed();
theTV.buttonSixPressed();
theTV.buttonNinePressed();

theTV2.buttonNinePressed();
