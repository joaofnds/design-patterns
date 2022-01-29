/**
 * What is the interpreter design pattern?
 *
 * - It is used to convert one representation of data into another
 * - The Context contains the information that will be interpreted
 * - The Expression is an abstract class that defines all the methods
 *   needed to perform the different conversions
 * - The Terminal or Concrete Expressions provide specific conversions on
 *   different types of data
 *
 * @see http://www.newthinktank.com/2012/10/interpreter-design-pattern-tutorial/
 */

class ConversionContext {
  readonly from: keyof Expression;
  readonly to: keyof Expression;
  readonly quantity: number;

  public constructor(question: string) {
    const components = question.split(" ");
    this.from = this.capitalize(
      this.sanitize(components[1])
    ) as keyof Expression;
    this.to = this.sanitize(components[3]) as keyof Expression;
    this.quantity = Number(components[0]);
  }

  private capitalize(str: string): string {
    return str.charAt(0).toUpperCase() + str.slice(1);
  }

  private sanitize(str: string): string {
    const lowerStr = str.toLowerCase();
    if (lowerStr.charAt(str.length - 1) === "s") {
      return lowerStr;
    } else {
      return lowerStr + "s";
    }
  }
}

abstract class Expression {
  abstract bits(quantity: number): string;
  abstract nibbles(quantity: number): string;
  abstract bytes(quantity: number): string;
}

class Bit extends Expression {
  bits(quantity: number): string {
    return String(quantity);
  }

  nibbles(quantity: number): string {
    return String(Math.floor(quantity / 4));
  }

  bytes(quantity: number): string {
    return String(Math.floor(quantity / 8));
  }
}

class Nibble extends Expression {
  bits(quantity: number): string {
    return String(quantity * 4);
  }

  nibbles(quantity: number): string {
    return String(quantity);
  }

  bytes(quantity: number): string {
    return String(Math.floor(quantity / 2));
  }
}

class Byte extends Expression {
  bits(quantity: number): string {
    return String(quantity * 8);
  }

  nibbles(quantity: number): string {
    return String(quantity * 2);
  }

  bytes(quantity: number): string {
    return String(quantity);
  }
}

//---------------------------------------------

function getClassFromInput(input: string) {
  switch (input) {
    case "Bits":
      return new Bit();
    case "Bytes":
      return new Byte();
    case "Nibbles":
      return new Nibble();
    default:
      throw new Error(`unknown input class '${input}'`);
  }
}

const questionAsked = "8 bits to bytes";
const { from, to, quantity } = new ConversionContext(questionAsked);

const result = getClassFromInput(from)[to](quantity);
console.log(`${quantity} ${from} is ${result} ${to}`);
