module Queue =
  struct
    type 'a queue = ('a list) ref
    let empty(): 'a queue = ref []
  end
