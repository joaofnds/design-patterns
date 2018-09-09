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
