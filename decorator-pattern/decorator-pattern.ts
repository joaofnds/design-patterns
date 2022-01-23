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
  readonly description: string;
  readonly price: number;
}

class PlainPizza implements Pizza {
  readonly description = "Thin Dough";
  readonly price = 400;
}

abstract class ToppingDecorator implements Pizza {
  constructor(protected pizza: Pizza) {}

  get description(): string {
    return this.pizza.description;
  }

  get price(): number {
    return this.pizza.price;
  }
}

class Mozzarella extends ToppingDecorator {
  get description(): string {
    return this.pizza.description + ", Mozzarella";
  }

  get price(): number {
    return this.pizza.price + 50;
  }
}

class TomatoSauce extends ToppingDecorator {
  get description(): string {
    return this.pizza.description + ", Tomato Sauce";
  }

  get price(): number {
    return this.pizza.price + 35;
  }
}

//-----------------------------------------------------------------

const pizza = new TomatoSauce(new Mozzarella(new PlainPizza()));
console.log("ingredients:", pizza.description);
console.log("price:", pizza.price);
