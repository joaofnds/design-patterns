/**
 * What is the Factory Pattern?
 *
 * - When a method return one of several possible classes that
 *   share a common super class
 * Ex:
 *   - Create a new enemy in a game
 *   - Random number generator picks a number assigned to a specific enemy
 *   - The factory returns the enemy associated with that number
 * - The class is chosen at run time
 *
 * @see http://www.newthinktank.com/2012/09/factory-design-pattern-tutorial/
 */

abstract class EnemyShip {
  private name: string;
  private amtDamage: number;

  public getName() {
    return this.name;
  }

  public setName(name: string) {
    this.name = name;
  }

  public getDamage() {
    return this.amtDamage;
  }

  public setDamage(damage: number) {
    this.amtDamage = damage;
  }

  public followHeroShip() {
    console.log(this.getName(), "is following the hero");
  }

  public displayEnemyShip() {
    console.log(this.getName(), "is on the screen");
  }

  public enemyShipShoots() {
    console.log(this.getName(), "attacks and does", this.getDamage(), "damage");
  }
}

class UFOEnemyShip extends EnemyShip {
  constructor() {
    super();
    this.setName("UFO Enemy Ship");
    this.setDamage(20.0);
  }
}

class RocketEnemyShip extends EnemyShip {
  constructor() {
    super();
    this.setName("Rocker Enemy Ship");
    this.setDamage(10.0);
  }
}

abstract class EnemyShipFactory {
  public static make(type: string): EnemyShip {
    switch (type) {
      case "ufo":
        return new UFOEnemyShip();
      case "rocket":
        return new RocketEnemyShip();
      default:
        throw new Error("Unknown enemy ship");
    }
  }
}

console.log(EnemyShipFactory.make("rocket").displayEnemyShip());
