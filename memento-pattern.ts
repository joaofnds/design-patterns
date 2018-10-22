/**
 * What is the Memento Design Pattern?
 * - Used to store an object state at a point in time so it can be
 *   returned to that state later.
 * - It simply allows you to undo/redo changes on an Object
 * - Memento: The basic object that is stored in different states
 * - Originator: Sets and Gets values from the currently targeted
 *   Memento. Create new Mementos and assigns current values to them
 * - Caretaker: Holds a List that contains all previous versions of the Memento.
 *   It can store and retrieve stored Mementos
 *
 * @see http://www.newthinktank.com/2012/10/memento-design-pattern-tutorial/
 */

class Memento {
  private article: string;
  public constructor(articleSave: string) { this.article = articleSave; };
  public getSavedArticle(): string { return this.article; };
}

class Originator {
  private article: string;

  public set(newArticle: string): void {
    console.log(`[Originator] current version of article:\n${newArticle}`);
    this.article = newArticle;
  }

  public storeInMemento(): Memento {
    console.log(`[Originator] saving to memento`)
    return new Memento(this.article);
  }

  public restoreFromMemento(memento: Memento): string {
    this.article = memento.getSavedArticle();
    console.log(`[Originator] previous article saved in Memento:\n${this.article}`);
    return this.article;
  }
}

class CareTaker {
  private savedArticles: Array<Memento> = [];

  public addMemento(memento: Memento): void {
    this.savedArticles.push(memento);
  }

  public getMemento(index: number): Memento {
    return this.savedArticles[index];
  }
}

//------------------------------------------------------------------------------

// const careTaker: CareTaker = new CareTaker();
// const originator: Originator = new Originator();

class Editor {
  public text: string;
  private originator: Originator;
  private careTaker: CareTaker;

  private saveFiles: number;
  private currentArticle: number;

  public constructor() {
    this.text = ""
    this.originator = new Originator();
    this.careTaker = new CareTaker();
    this.saveFiles = 0;
    this.currentArticle = 0;
  }

  public save() {
    this.originator.set(this.text);
    this.careTaker.addMemento(this.originator.storeInMemento());
    this.saveFiles++;
    this.currentArticle++;
  }

  public undo() {
    if (this.currentArticle >= 1) {
      this.currentArticle--;
      this.text = this.originator.restoreFromMemento(this.careTaker.getMemento(this.currentArticle))
    }
  }

  public redo() {
    if ((this.saveFiles - 1) > this.currentArticle) {
      this.currentArticle++;
      this.text = this.originator.restoreFromMemento(this.careTaker.getMemento(this.currentArticle))
    }
  }
}

const editor: Editor = new Editor;
['foo', 'bar', 'baz'].forEach(text => {
  editor.text = text;
  editor.save();
});

const f = [];
f.push(editor.text)
editor.undo()
f.push(editor.text)
editor.undo()
f.push(editor.text)
editor.undo()
f.push(editor.text)
editor.redo()
f.push(editor.text)
editor.redo()
f.push(editor.text)
editor.redo()
console.log(f)