/**
 * When to use the Observer Pattern
 *
 * - when you need many other object to receive an update when
 *   another object changes
 * Ex:
 *   - Stock market with thousands of stocks need to send updates to
 *     objects representing individual stocks
 *   - The Subject(publisher) sends many stocks to the observers
 *   - The Observers(subscribers) takes the ones they want and use them
 *
 * @see http://www.newthinktank.com/2012/08/observer-design-pattern-tutorial/
 */

interface Subject {
  register(o: Observer): void;
  unregister(o: Observer): void;
  notifyObservers(): void;
}

interface Observer {
  update(stocks: Object): void;
}

class DuplicateStockError extends Error {
  constructor(name: string) {
    super(`stock '${name}' already exists`);
  }
}

class StockNotFoundError extends Error {
  constructor(name: string) {
    super(`stock '${name}' doens't exists`);
  }
}

class StockGrabber implements Subject {
  private observers = new Array<Observer>();
  private stocks: Record<string, number> = {};

  register(o: Observer): void {
    this.observers.push(o);
  }

  unregister(o: Observer): void {
    const index = this.observers.indexOf(o);
    this.observers.splice(index, 1);
  }

  notifyObservers(): void {
    const stocksCopy = JSON.parse(JSON.stringify(this.stocks));
    this.observers.forEach((o) => o.update(stocksCopy));
  }

  addStock(name: string, price: number) {
    if (this.hasStock(name)) throw new DuplicateStockError(name);

    this.stocks[name] = price;
    this.notifyObservers();
  }

  removeStock(name: string) {
    if (!this.hasStock(name)) throw new StockNotFoundError(name);

    delete this.stocks[name];
    this.notifyObservers();
  }

  updateStock(name: string, price: number) {
    if (!this.hasStock(name)) throw new StockNotFoundError(name);

    this.stocks[name] = price;
    this.notifyObservers();
  }

  private hasStock(name: string) {
    return name in this.stocks;
  }
}

class StockObserver implements Observer {
  private stocks = new Object();

  constructor(grabber: StockGrabber) {
    grabber.register(this);
  }

  update(stocks: Object): void {
    console.log("got an update", { from: this.stocks, to: stocks });
    this.stocks = stocks;
  }
}

const grabber: StockGrabber = new StockGrabber();
const ob1: StockObserver = new StockObserver(grabber);
const ob2: StockObserver = new StockObserver(grabber);

grabber.addStock("foo", 123);
grabber.addStock("bar", 123);
grabber.addStock("baz", 123);
