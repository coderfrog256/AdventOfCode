defmodule SantaActor do
  use Agent

  def start_link() do
    Agent.start_link(fn -> %{:x => 0, :y => 0, :visited => %{{0, 0} => 1}} end)
  end

  def move(self, x, y) do
    Agent.update(self, fn state ->
      x = Map.get(state, :x) + x
      y = Map.get(state, :y) + y
      %{:x => x, :y => y, :visited => Map.put(Map.get(state, :visited), {x, y}, 1)}
    end)
  end

  def get_visited(self) do
    Agent.get(self, fn state -> Map.get(state, :visited) end)
  end
end

defmodule Day3 do
  def scenario_one() do
    {:ok, santa} = SantaActor.start_link()
    {:ok, file} = File.read("../input/day3.txt")

    String.graphemes(file)
    |> Enum.map(fn command ->
      case command do
        ">" -> SantaActor.move(santa, 1, 0)
        "<" -> SantaActor.move(santa, -1, 0)
        "^" -> SantaActor.move(santa, 0, 1)
        "v" -> SantaActor.move(santa, 0, -1)
        _ -> raise "Unexpected command #{inspect(command)}"
      end
    end)

    houses = SantaActor.get_visited(santa)
    length(Map.keys(houses))
  end

  def scenario_two() do
    {:ok, real} = SantaActor.start_link()
    {:ok, robo} = SantaActor.start_link()
    {:ok, file} = File.read("../input/day3.txt")

    Enum.with_index(String.graphemes(file))
    |> Enum.map(fn {command, index} ->
      actor =
        if rem(index, 2) == 0 do
          real
        else
          robo
        end

      case command do
        ">" -> SantaActor.move(actor, 1, 0)
        "<" -> SantaActor.move(actor, -1, 0)
        "^" -> SantaActor.move(actor, 0, 1)
        "v" -> SantaActor.move(actor, 0, -1)
        _ -> raise "Unexpected command #{inspect(command)}"
      end
    end)

    real_houses = SantaActor.get_visited(real)
    robo_houses = SantaActor.get_visited(robo)
    houses = Map.merge(real_houses, robo_houses)
    length(Map.keys(houses))
  end
end

IO.puts("Scenario One: #{inspect(Day3.scenario_one())}")
IO.puts("Scenario Two: #{inspect(Day3.scenario_two())}")
