type 'a ntree = Tr of 'a * 'a ntree list;;

let result = Tr(1, []);;

result;;