defmodule Day5 do
  # Optimistic parse. If we parsed a (\d+), we know it's an int.
  defp parse_int(int) do
    {parsed, _} = Integer.parse(int)
    parsed
  end

  defp build_crates(crate_description) do
    crate_description
    |> String.split("\n")
    |> Stream.filter(&String.contains?(&1, "["))
    |> Stream.flat_map(fn line ->
      line
      |> String.graphemes()
      |> Stream.chunk_every(4)
      |> Stream.map(&Enum.at(&1, 1))
      |> Stream.with_index(1)
    end)
    |> Stream.filter(&(elem(&1, 0) != " "))
    |> Enum.reduce(Map.new(), fn create_with_index, map ->
      {crate, index} = create_with_index
      stack = Map.get(map, index, [])
      Map.put(map, index, List.insert_at(stack, 0, crate))
    end)
  end

  defp run_commands(crates, commands) do
    commands
    |> String.split("\n")
    |> Enum.reduce(crates, fn command, crates ->
      [_, count, from, to] = Regex.run(~r/move (\d+) from (\d+) to (\d+)/, command)
      count = parse_int(count)
      from = parse_int(from)
      to = parse_int(to)

      Enum.reduce(0..(count - 1), crates, fn _, crates ->
        {popped, from_stack} = List.pop_at(Map.get(crates, from), -1)
        to_stack = List.insert_at(Map.get(crates, to), -1, popped)
        crates = Map.put(crates, from, from_stack)
        Map.put(crates, to, to_stack)
      end)
    end)
    |> Stream.map(&elem(&1, 1))
    |> Stream.map(&List.last(&1))
    |> Enum.reduce("", &(&2 <> &1))
  end

  defp run_commands_multimove(crates, commands) do
    commands
    |> String.split("\n")
    |> Enum.reduce(crates, fn command, crates ->
      [_, count, from, to] = Regex.run(~r/move (\d+) from (\d+) to (\d+)/, command)
      count = parse_int(count)
      from = parse_int(from)
      to = parse_int(to)

      from_stack = Map.get(crates, from)
      to_move = Enum.slice(from_stack, -1 * count, count)
      from_stack = Enum.slice(from_stack, 0, length(from_stack) - count)
      to_stack = Map.get(crates, to) ++ to_move

      crates = Map.put(crates, from, from_stack)
      Map.put(crates, to, to_stack)
    end)
    |> Stream.map(&elem(&1, 1))
    |> Stream.map(&List.last(&1))
    |> Enum.reduce("", &(&2 <> &1))
  end

  def scenario_one() do
    [crate_description, commands] = File.read!("../input/day5.txt") |> String.split("\n\n")
    crates = build_crates(crate_description)
    run_commands(crates, commands)
  end

  def scenario_two() do
    [crate_description, commands] = File.read!("../input/day5.txt") |> String.split("\n\n")
    crates = build_crates(crate_description)
    run_commands_multimove(crates, commands)
  end
end

IO.puts("Scenario One: #{inspect(Day5.scenario_one())}")
IO.puts("Scenario Two: #{inspect(Day5.scenario_two())}")
