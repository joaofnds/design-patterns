class Specification
  def satisfied_by?(candidate)
    raise NotImplementedError
  end

  def and(other)
    AndSpecification.new(self, other)
  end

  def or(other)
    OrSpecification.new(self, other)
  end

  def not
    NotSpecification.new(self)
  end
end

class AndSpecification < Specification
  def satisfied_by?(candidate)
    a.satisfied_by?(candidate) && b.satisfied_by?(candidate)
  end

  private

  attr_reader :a, :b

  def initialize(a, b)
    @a = a
    @b = b
  end
end

class OrSpecification < Specification
  def satisfied_by?(candidate)
    a.satisfied_by?(candidate) || b.satisfied_by?(candidate)
  end

  private

  attr_reader :a, :b

  def initialize(a, b)
    @a = a
    @b = b
  end
end

class NotSpecification < Specification
  def satisfied_by?(candidate)
    !spec.satisfied_by?(candidate)
  end

  private

  attr_reader :spec

  def initialize(spec)
    @spec = spec
  end
end
