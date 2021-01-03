require_relative 'specification'

describe Specification do
  it { is_expected.to respond_to(:satisfied_by?) }
  it { is_expected.to respond_to(:and) }
  it { is_expected.to respond_to(:or) }
  it { is_expected.to respond_to(:not) }
end

describe Specification, :satisfied_by? do
  subject { PersonMinAgeSpec.new(21) }

  let(:person_with_20yo) { Person.new('a', 20) }
  let(:person_with_21yo) { Person.new('b', 21) }
  let(:person_with_22yo) { Person.new('c', 22) }

  it { is_expected.to_not be_satisfied_by(person_with_20yo) }
  it { is_expected.to be_satisfied_by(person_with_21yo) }
  it { is_expected.to be_satisfied_by(person_with_22yo) }
end

describe Specification, :and do
  let(:name_joao) { PersonNameSpec.new('joao') }
  let(:min_21yo) { PersonMinAgeSpec.new(21) }

  subject { name_joao.and min_21yo }

  it { is_expected.to_not be_satisfied_by Person.new('bob', 20) }
  it { is_expected.to_not be_satisfied_by Person.new('bob', 21)  }
  it { is_expected.to_not be_satisfied_by Person.new('joao', 20) }
  it { is_expected.to be_satisfied_by Person.new('joao', 21) }
end

describe Specification, :and do
  let(:name_joao) { PersonNameSpec.new('joao') }
  let(:min_21yo) { PersonMinAgeSpec.new(21) }

  subject { name_joao.or min_21yo }

  it { is_expected.to_not be_satisfied_by Person.new('bob', 20) }
  it { is_expected.to be_satisfied_by Person.new('bob', 21)  }
  it { is_expected.to be_satisfied_by Person.new('joao', 20) }
  it { is_expected.to be_satisfied_by Person.new('joao', 21) }
end

describe Specification, :not do
  subject { PersonMinAgeSpec.new(21).not }

  it { is_expected.to be_satisfied_by Person.new('joao', 20) }
  it { is_expected.to_not be_satisfied_by Person.new('joao', 21) }
end

class PersonMinAgeSpec < Specification
  def satisfied_by?(person)
    person.age >= age
  end

  private

  attr_reader :age

  def initialize(age)
    @age = age
  end
end

class PersonNameSpec < Specification
  def satisfied_by?(person)
    person.name == name
  end

  private

  attr_reader :name

  def initialize(name)
    @name = name
  end
end

class Person
  attr_reader :name, :age

  private

  def initialize(name, age)
    @name = name
    @age = age
  end
end
