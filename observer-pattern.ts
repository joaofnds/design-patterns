interface Subject {
  register(o: Observer): void;
  unregister(o: Observer): void;
  notifyObservers(): void;
}

interface Observer {
  update(stocks: Object): void;
}

class StockGrabber implements Subject {
  private observers: Array<Observer>;
  private stocks: Object;

  constructor() {
    this.observers = new Array();
    this.stocks = new Array();
  }

  register(o: Observer): void {
    this.observers.push(o);
  }

  unregister(o: Observer): void {
    const index = this.observers.indexOf(o);
    this.observers.splice(index, 1);
  }

  notifyObservers(): void {
    this.observers.forEach(o => o.update(this.stocks));
  }

  addStock(name: string, price: number) {
    if (this.stocks[name] !== undefined) throw new Error("key already exist");

    this.stocks[name] = price;
    this.notifyObservers();
  }

  removeStock(name: string) {
    delete this.stocks[name];
    this.notifyObservers();
  }

  updateStock(name: string, price: number) {
    if (this.stocks[name] !== undefined) throw new Error("stock doesn't exist");

    this.stocks[name] = price;
    this.notifyObservers();
  }
}

class StockObserver implements Observer {
  private stocks: Object;
  private grabber: StockGrabber;

  constructor(grabber: StockGrabber) {
    this.stocks = new Object();
    this.grabber = grabber;
    grabber.register(this);
  }

  update(stocks: Object): void {
    this.stocks = stocks;
  }
}

const grabber: StockGrabber = new StockGrabber();
const ob1: StockObserver = new StockObserver(grabber);
const ob2: StockObserver = new StockObserver(grabber);

grabber.addStock("foo", 123);
grabber.addStock("bar", 123);
grabber.addStock("baz", 123);
