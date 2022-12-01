defmodule Day1 do
  def build_elves(lines) do
    lines
    |> Enum.chunk_by(&(&1 == ""))
    |> Enum.map(fn chunk ->
      chunk
      |> Enum.filter(&(&1 != ""))
      |> Enum.map(&String.to_integer/1)
      |> Enum.sum()
    end)
  end

  def scenario_one() do
    File.read!("../input/day1.txt")
    |> String.split("\n")
    |> Day1.build_elves()
    |> Enum.max()
  end

  def scenario_two() do
    File.read!("../input/day1.txt")
    |> String.split("\n")
    |> Day1.build_elves()
    |> Enum.sort(&>/2)
    |> Enum.take(3)
    |> Enum.sum()
  end
end

IO.puts("Scenario One: #{inspect(Day1.scenario_one())}")
IO.puts("Scenario Two: #{inspect(Day1.scenario_two())}")
