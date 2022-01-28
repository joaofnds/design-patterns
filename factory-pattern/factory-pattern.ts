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
  constructor(private name: string, private damage: number) {}

  public followHeroShip() {
    console.log(this.name, "is following the hero");
  }

  public displayEnemyShip() {
    console.log(this.name, "is on the screen");
  }

  public enemyShipShoots() {
    console.log(this.name, "attacks and does", this.damage, "damage");
  }
}

class UFOEnemyShip extends EnemyShip {
  constructor() {
    super("UFO Enemy Ship", 20.0);
  }
}

class RocketEnemyShip extends EnemyShip {
  constructor() {
    super("Rocker Enemy Ship", 10.0);
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

// ------------------------------------------------------------------------------

const ship = EnemyShipFactory.make("rocket");
ship.displayEnemyShip();
ship.followHeroShip();
ship.enemyShipShoots();
