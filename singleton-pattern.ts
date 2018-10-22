/**
 * What is the Singleton Pattern?
 *
 * It is used when you want to eliminate the option of instantiating
 * more than one object
 *
 * @see http://www.newthinktank.com/2012/09/singleton-design-pattern-tutorial/
 */

class Singleton {
  static firstInstance: Singleton = null;

  private constructor() { }

  static getInstance(): Singleton {
    if (this.firstInstance === null) {
      this.firstInstance = new Singleton();
    }

    return this.firstInstance;
  }
}
