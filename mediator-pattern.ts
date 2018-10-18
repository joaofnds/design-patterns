/**
 * What is the Mediator Design Pattern?
 *
 * - It is used to handle communication between related objects
 * - All communication is handled by the Mediator and the Colleagues don't need to know anything about each other
 * - GOF: Allows loose coupling by encapsulating the way disparate sets of objects interact and communicate with each other. Allows for the actions of each object set to vary independently of one another.
 */

class StockOffer {
  private stockShares: number = 0;
  private stockSymbol: string = "";
  private colleagueCode: number = 0;

  public constructor(
    stockShares: number,
    stockSymbol: string,
    colleagueCode: number
  ) {
    this.stockShares = stockShares;
    this.stockSymbol = stockSymbol;
    this.colleagueCode = colleagueCode;
  }

  public getStockShares(): number {
    return this.stockShares;
  }

  public getStockSymbol(): string {
    return this.stockSymbol;
  }

  public getColleagueCode(): number {
    return this.colleagueCode;
  }
}

abstract class Colleague {
  private mediator: Mediator;
  private colleagueCode: number;

  public constructor(mediator: Mediator) {
    this.mediator = mediator;
    this.mediator.addColleague(this);
  }

  public saleOffer(stock: string, shares: number): void {
    this.mediator.saleOffer(stock, shares, this.colleagueCode);
  }

  public buyOffer(stock: string, shares: number): void {
    this.mediator.buyOffer(stock, shares, this.colleagueCode);
  }

  public setColleagueCode(colleagueCode: number): void {
    this.colleagueCode = colleagueCode;
  }
}

class GormanSlacks extends Colleague {
  public constructor(mediator: Mediator) {
    super(mediator);
    console.log("Gorman Slacks signed up for the exchange");
  }
}

class JTPoorman extends Colleague {
  public constructor(mediator: Mediator) {
    super(mediator);
    console.log("JTPoormanSlacks Slacks signed up for the exchange");
  }
}

interface Mediator {
  saleOffer(stock: string, shares: number, colleagueCode: number): void;
  buyOffer(stock: string, shares: number, colleagueCode: number): void;
  addColleague(colleague: Colleague): void;
}

class StockMediator implements Mediator {
  private colleagues: { [key: number]: Colleague };
  private stockBuyOffers: Array<StockOffer>;
  private stockSaleOffers: Array<StockOffer>;

  private colleagueCode: number = 0;

  public constructor() {
    this.colleagueCode = 0;
    this.colleagues = {};
    this.stockBuyOffers = [];
    this.stockSaleOffers = [];
  }

  saleOffer(stock: string, shares: number, colleagueCode: number): void {
    let stockSold: boolean = false;

    for (let offer of this.stockBuyOffers) {
      if (
        offer.getStockSymbol() === "stock" &&
        offer.getStockShares() === shares
      ) {
        console.log(
          `${shares} shares of ${stock} sold to colleague code ${colleagueCode}`
        );
        this.stockBuyOffers.splice(this.stockBuyOffers.indexOf(offer), 1);
        stockSold = true;
        break;
      }
    }

    if (!stockSold) {
      console.log(`${shares} shares of ${stock} added to inventory`);
      const newOffer: StockOffer = new StockOffer(shares, stock, colleagueCode);
      this.stockSaleOffers.push(newOffer);
    }
  }

  buyOffer(stock: string, shares: number, colleagueCode: number): void {
    let stockBought: boolean = false;

    for (let offer of this.stockSaleOffers) {
      if (
        offer.getStockSymbol() === stock &&
        offer.getStockShares() === shares
      ) {
        console.log(
          `${shares} shares of ${stock} bought by colleague code ${colleagueCode}`
        );

        this.stockSaleOffers.splice(this.stockSaleOffers.indexOf(offer), 1);
        stockBought = true;
        break;
      }
    }

    if (!stockBought) {
      console.log(`${shares} shares of ${stock} added to inventory`);
      const newOffer: StockOffer = new StockOffer(shares, stock, colleagueCode);
      this.stockBuyOffers.push(newOffer);
    }
  }

  addColleague(colleague: Colleague): void {
    this.colleagues[this.colleagueCode] = colleague;
    colleague.setColleagueCode(this.colleagueCode);
    this.colleagueCode++;
  }

  getStockOfferings(): void {
    console.log("Stocks for sale");
    for (let offer of this.stockSaleOffers) {
      console.log(`\t- ${offer.getStockShares()} of ${offer.getStockSymbol()}`);
    }

    console.log("Stocks buy offers:");
    for (let offer of this.stockBuyOffers) {
      console.log(`\t- ${offer.getStockShares()} of ${offer.getStockSymbol()}`);
    }
  }
}

//------------------------------------------------------------------------------

const nyse: StockMediator = new StockMediator();
const broker: GormanSlacks = new GormanSlacks(nyse);
const broker2: JTPoorman = new JTPoorman(nyse);

broker.saleOffer("MSFT", 100);
broker.saleOffer("GOOG", 50);
broker2.buyOffer("MSFT", 100);
broker2.saleOffer("NRG", 10);
broker.buyOffer("NRG", 10);
nyse.getStockOfferings();
