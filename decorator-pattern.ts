/**
 * What is the Decorator Pattern?
 *
 * - The Decorator allows you to modify an object dynamically
 * - You would use it when you want the capabilities of inheritance with
 *   subclasses, but you need to add functionality at run time
 * - It is more flexible than inheritance
 * - Simplifies code because you add functionality using many simple classes
 * - Rather then rewrite old code, you can extend with new code
 *
 * @see http://www.newthinktank.com/2012/09/decorator-design-pattern-tutorial/
 */

interface Pizza {
  getDescription(): string;
  getCost(): number;
}

class PlainPizza implements Pizza {
  getDescription(): string {
    return "Thin Dough";
  }
  getCost(): number {
    return 400;
  }
}

abstract class ToppingDecorator implements Pizza {
  protected tempPizza: Pizza;

  constructor(newPizza: Pizza) {
    this.tempPizza = newPizza;
  }

  getDescription(): string {
    return this.tempPizza.getDescription();
  }

  getCost(): number {
    return this.tempPizza.getCost();
  }
}

class Mozzarella extends ToppingDecorator {
  constructor(newPizza: Pizza) {
    super(newPizza);
    console.log("Adding Dough");
    console.log("Adding Moz");
  }

  getDescription(): string {
    return this.tempPizza.getDescription() + ", Mozzarella";
  }

  getCost(): number {
    return this.tempPizza.getCost() + 50;
  }
}

class TomatoSauce extends ToppingDecorator {
  constructor(newPizza: Pizza) {
    super(newPizza);
    console.log("Adding Sauce");
  }

  getDescription(): string {
    return this.tempPizza.getDescription() + ", Tomato Sauce";
  }

  getCost(): number {
    return this.tempPizza.getCost() + 35;
  }
}

//-----------------------------------------------------------------

const basicPizza = new TomatoSauce(new Mozzarella(new PlainPizza()));
console.log("ingredients", basicPizza.getDescription());
console.log("price", basicPizza.getCost());
