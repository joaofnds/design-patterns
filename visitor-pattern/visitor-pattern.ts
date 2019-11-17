/**
 * What is the visitor design pattern?
 *
 * - Allows you to add methods to classes of different types without much
 *   altering to those classes
 * - You can make completely different methods depending on the class used
 * - Allows you to define external classes that extend other classes without
 *   majorly editing them
 *
 * @see http://www.newthinktank.com/2012/11/visitor-design-pattern-tutorial/
 */

interface Visitor {
  visitLiquor(liquorItem: Liquor): number;
  visitTobacco(tobaccoItem: Tobacco): number;
  visitNecessity(necessityItem: Necessity): number;
}

class TaxVisitor implements Visitor {
  visitLiquor(liquorItem: Liquor): number {
    const tax = liquorItem.price * 0.18;
    return liquorItem.price + tax;
  }

  visitTobacco(tobaccoItem: Tobacco): number {
    const tax = tobaccoItem.price * 0.21;
    return tobaccoItem.price + tax;
  }

  visitNecessity(necessityItem: Necessity): number {
    const tax = 0;
    return necessityItem.price + tax;
  }
}

class TaxHolydayVisitor implements Visitor {
  visitLiquor(liquorItem: Liquor): number {
    const tax = liquorItem.price * 0.18 * 2;
    return liquorItem.price + tax;
  }

  visitTobacco(tobaccoItem: Tobacco): number {
    const tax = tobaccoItem.price * 0.21 * 2;
    return tobaccoItem.price + tax;
  }

  visitNecessity(necessityItem: Necessity): number {
    const tax = 0 * 2;
    return necessityItem.price + tax;
  }
}

interface Visitable {
  accept(visitor: Visitor): number;
}

class Liquor implements Visitable {
  price: number;

  constructor(price: number) {
    this.price = price;
  }

  accept(visitor: Visitor): number {
    return visitor.visitLiquor(this);
  }
}

class Necessity implements Visitable {
  price: number;

  constructor(price: number) {
    this.price = price;
  }

  accept(visitor: Visitor): number {
    return visitor.visitNecessity(this);
  }
}

class Tobacco implements Visitable {
  price: number;

  constructor(price: number) {
    this.price = price;
  }

  accept(visitor: Visitor): number {
    return visitor.visitTobacco(this);
  }
}

const taxVisitor = new TaxVisitor();
const taxHolydayVisitor = new TaxHolydayVisitor();

const liquor = new Liquor(10);
const tobacco = new Tobacco(10);
const necessity = new Necessity(10);

console.log("Normal Tax");
console.table({
  liquor: liquor.accept(taxVisitor),
  tobacco: tobacco.accept(taxVisitor),
  necessity: necessity.accept(taxVisitor)
});

/**
 * Normal Tax
 * ┌───────────┬────────┐
 * │  (index)  │ Values │
 * ├───────────┼────────┤
 * │  liquor   │  11.8  │
 * │  tobacco  │  12.1  │
 * │ necessity │   10   │
 * └───────────┴────────┘
 */

console.log("Holyday Tax");
console.table({
  liquor: liquor.accept(taxHolydayVisitor),
  tobacco: tobacco.accept(taxHolydayVisitor),
  necessity: necessity.accept(taxHolydayVisitor)
});

/**
 * Holyday Tax
 * ┌───────────┬────────┐
 * │  (index)  │ Values │
 * ├───────────┼────────┤
 * │  liquor   │  13.6  │
 * │  tobacco  │  14.2  │
 * │ necessity │   10   │
 * └───────────┴────────┘
 */
