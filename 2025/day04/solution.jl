using DSP

function readInput()
    dict = Dict('@' => 1, '.' => 0)
    chars = collect.(readlines("input.txt"))
    charM = reduce(hcat, chars)
    M = getindex.(Ref(dict), charM)
end

function removeRolls(M)
  kernel = [-1 -1 -1; -1 4 -1; -1 -1 -1]
  removable = conv(M, kernel)[2:end-1, 2:end-1]
  clamp!(removable, 0, 1)
  M .-= removable
  sum(removable)
end

function removeAll(M)
  removed = 1
  totalRemoved = 0
  print("Removed:")
  while removed > 0
      removed = removeRolls(M)
      print(" $(removed),")
      totalRemoved += removed
  end
  println()
  println("Total: $(totalRemoved)")
end

M = readInput()
removeAll(M)
