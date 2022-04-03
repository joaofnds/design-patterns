/**
 * What is the State Design Pattern?
 *
 * - Allows an object to alter its behavior when its internal state changes.
 *   The object will appear to change its class
 * - Context (account): Maintains an instance of a ConcreateState subclass
 *   that defines the current state
 * - State: Defines an interface for encapsulating the behavior associated
 *   with a particular state of the Context
 * - Concrete State: Each subclass implements a behavior associated with
 *   a state of Context
 *
 * @see http://www.newthinktank.com/2012/10/state-design-pattern-tutorial/
 */

interface ATMState {
  insertCard(): void;
  ejectCard(): void;
  insertPin(pin: number): void;
  requestCash(amout: number): void;
}

class HasCard implements ATMState {
  public constructor(private readonly atm: ATMMachine) {}

  insertCard() {
    console.error("Card tray full");
  }

  ejectCard() {
    this.atm.setNoCardState();
    console.log("Card ejected");
  }

  insertPin(pinEntered: number) {
    if (pinEntered === 1234) {
      this.atm.setHasPin();
    } else {
      console.error("Wrong PIN");
      this.ejectCard();
    }
  }

  requestCash(_amount: number) {
    console.error("Enter PIN first");
  }
}

class NoCard implements ATMState {
  public constructor(private readonly atm: ATMMachine) {}

  insertCard() {
    console.log("Please enter a PIN");
    this.atm.setHasCardState();
  }

  ejectCard() {
    console.error("Enter a card first");
  }

  insertPin(_pin: number) {
    console.error("Enter a card first");
  }

  requestCash(_amount: number) {
    console.error("Enter a card first");
  }
}

class HasPin implements ATMState {
  public constructor(private readonly atm: ATMMachine) {}

  insertCard() {
    console.error("Card tray full");
  }

  ejectCard() {
    this.atm.setNoCardState();
    console.log("Card ejected");
  }

  insertPin(_pin: number) {
    console.log("Already entered PIN");
  }

  requestCash(amount: number) {
    if (amount > this.atm.cashAvailable) {
      console.error("Don't have that cash");
    } else {
      console.log(`${amount} is provided by the machine`);
      this.atm.cashAvailable = this.atm.cashAvailable - amount;

      if (this.atm.cashAvailable <= 0) {
        this.atm.setNoCashState();
      }
    }

    this.ejectCard();
  }
}

class NoCash implements ATMState {
  public constructor(_atm: ATMMachine) {}

  insertCard() {
    console.error("No money");
  }

  ejectCard() {
    console.error("No money");
    console.error("You didn't enter a card");
  }

  insertPin(_pin: number) {
    console.error("No money");
  }

  requestCash(_amount: number) {
    console.error("No money");
  }
}

class ATMMachine {
  private readonly hasCard: ATMState;
  private readonly noCard: ATMState;
  private readonly hasCorrectPin: ATMState;
  private readonly atmOutOfMoney: ATMState;
  private state: ATMState;

  cashAvailable = 2000;

  public constructor() {
    this.hasCard = new HasCard(this);
    this.noCard = new NoCard(this);
    this.hasCorrectPin = new HasPin(this);
    this.atmOutOfMoney = new NoCash(this);

    this.state = this.noCard;

    if (this.cashAvailable < 0) {
      this.state = this.atmOutOfMoney;
    }
  }

  public insertCard() {
    this.state.insertCard();
  }

  public ejectCard() {
    this.state.ejectCard();
  }

  public requestCash(amount: number) {
    this.state.requestCash(amount);
  }

  public insertPin(pin: number) {
    this.state.insertPin(pin);
  }

  public setHasCardState() {
    this.state = this.hasCard;
  }

  public setNoCardState() {
    this.state = this.noCard;
  }

  public setHasPin() {
    this.state = this.hasCorrectPin;
  }

  public setNoCashState() {
    this.state = this.atmOutOfMoney;
  }
}

//-------------------------------------------------------------------------

const atmMachine: ATMMachine = new ATMMachine();
atmMachine.insertCard();
atmMachine.ejectCard();
atmMachine.insertCard();
atmMachine.insertPin(1234);
atmMachine.requestCash(2000);
atmMachine.insertCard();
atmMachine.insertPin(1234);
atmMachine.requestCash(2000);
