/**
 * What is the chain of responsibility design pattern?
 *
 * - This pattern send data to an object and if that object can't use it,
 * it sends it to any number of other objects that may be able to use it
 *    - Create 4 objects that can either add, subtract, multiply, or divide
 *    - Send 2 numbers and a command and allow these 4 object to decide
 *      which can handle the requested calculation
 *
 * @see http://www.newthinktank.com/2012/10/chain-of-responsibility-design-pattern-tutorial/
 */

interface Chain {
  setNextChain(nextChain: Chain): void;
  calculate(request: NumberRequest): void;
}

class NumberRequest {
  public constructor(
    readonly a: number,
    readonly b: number,
    readonly operation: string
  ) {}
}

class AddNumbers implements Chain {
  public nextInChain: Chain;

  setNextChain(nextChain: Chain) {
    this.nextInChain = nextChain;
  }

  calculate(request: NumberRequest) {
    if (request.operation == "add") {
      console.log(`${request.a} + ${request.b} = ${request.a + request.b}`);
    } else {
      this.nextInChain.calculate(request);
    }
  }
}

class SubtractNumbers implements Chain {
  private nextInChain: Chain;

  setNextChain(nextChain: Chain) {
    this.nextInChain = nextChain;
  }

  calculate(request: NumberRequest) {
    if (request.operation == "subtract") {
      console.log(`${request.a} - ${request.b} = ${request.a - request.b}`);
    } else {
      this.nextInChain.calculate(request);
    }
  }
}

class MultiplyNumbers implements Chain {
  private nextInChain: Chain;

  setNextChain(nextChain: Chain) {
    this.nextInChain = nextChain;
  }

  calculate(request: NumberRequest) {
    if (request.operation == "multiply") {
      console.log(`${request.a} * ${request.b} = ${request.a * request.b}`);
    } else {
      this.nextInChain.calculate(request);
    }
  }
}

class DivideNumbers implements Chain {
  private nextInChain: Chain;

  setNextChain(nextChain: Chain) {
    this.nextInChain = nextChain;
  }

  calculate(request: NumberRequest) {
    if (request.operation == "divide") {
      console.log(`${request.a} / ${request.b} = ${request.a / request.b}`);
    } else {
      this.nextInChain.calculate(request);
    }
  }
}

// --------------------------------------------------------

const chainCalc1: Chain = new AddNumbers();
const chainCalc2: Chain = new SubtractNumbers();
const chainCalc3: Chain = new MultiplyNumbers();
const chainCalc4: Chain = new DivideNumbers();

chainCalc1.setNextChain(chainCalc2);
chainCalc2.setNextChain(chainCalc3);
chainCalc3.setNextChain(chainCalc4);

const request = new NumberRequest(4, 2, "subtract");

chainCalc1.calculate(request);
