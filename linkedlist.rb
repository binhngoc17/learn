=begin
Util classes
=end
class LinkedListNode
  attr_accessor :value, :next_node

  def initialize(value, next_node=nil)
    @value = value
    @next_node = next_node
  end
end

def print_values(list_node)
  print "#{list_node.value} --> "
  if list_node.next_node.nil?
    print "nil\n"
    return
  else
    print_values(list_node.next_node)
  end
end

class Stack
  attr_reader :data

  def initialize
    @data = nil
  end

  # Push an item onto the stack
  def push(element)
    @data = LinkedListNode.new(element, @data)
  end

  # Pop an item off the stack.  
  # Remove the last item that was pushed onto the
  # stack and return it to the user
  def pop
    if not @data
      nil
    else
      element = @data.value
      @data = @data.next_node
      element
    end
  end

end

# Data
node1 = LinkedListNode.new(37)
node2 = LinkedListNode.new(99, node1)
node3 = LinkedListNode.new(12, node2)

=begin
Problem 1: Reverse list with a stack
=end
def reverse_list(list)
  reversed_list = nil

  while list
    reversed_list = LinkedListNode.new(list.value, reversed_list)
    list = list.next_node
  end

  reversed_list

end

puts print_values(reverse_list(node3))

=begin
Problem 2: Reverse list with mutation
=end

def reverse_list_with_mutation(list, previous=nil)

  while not list.nil?
    next_node = list.next_node
    list.next_node = previous
    previous = list
    list = next_node
  end

  previous

end

puts print_values(reverse_list_with_mutation(node3))

# def reverse_list_recursive(list, previous=nil)
#   if list.nil?
#     previous
#   else
#     reverse_list_recursive(list.next_node, LinkedListNode.new(list.value, previous))
#   end
# end

# puts print_values(reverse_list_recursive(node3))

=begin
Problem 3: Detect circle in a list
=end

def detect_circle(list)
  first_pointer  = list
  second_pointer = list
  is_circular = false

  while true
    # If we cannot move the pointer anymore then there is no circle in the linkedlist
    if not (first_pointer and first_pointer.next_node)
      is_circular = false
      break
    end
    if not (second_pointer and second_pointer.next_node and second_pointer.next_node.next_node)
      is_circular = false
      break
    end

    first_pointer = first_pointer.next_node
    second_pointer = second_pointer.next_node.next_node

    # If the two pointer meets, then there is a circle in the list
    if first_pointer == second_pointer
      is_circular = true
      break
    end
  end
  is_circular
end

def assert(a, b)
  if a != b 
    raise "Assert fails"
  end
end

# Test the non-circular linkedlist
assert(detect_circle(node3), false)

# Test circular list at the beginning
node1 = LinkedListNode.new(37)
node2 = LinkedListNode.new(99, node1)
node3 = LinkedListNode.new(12, node2)
node1.next_node = node3

assert(detect_circle(node3), true)

# Test there is a circle in the list
node1 = LinkedListNode.new(37)
node2 = LinkedListNode.new(99, node1)
node3 = LinkedListNode.new(12, node2)
node4 = LinkedListNode.new(12, node3)
node5 = LinkedListNode.new(12, node4)
node6 = LinkedListNode.new(12, node5)
node7 = LinkedListNode.new(12, node6)
node1.next_node = node4

assert(detect_circle(node7), true)


