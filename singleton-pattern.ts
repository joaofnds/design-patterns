class Singleton {
  static firstInstance: Singleton = null;

  private constructor() {}

  static getInstance(): Singleton {
    if (this.firstInstance === null) {
      this.firstInstance = new Singleton();
    }

    return this.firstInstance;
  }
}
