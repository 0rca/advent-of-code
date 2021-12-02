defmodule Day1 do
  @moduledoc """
  Run with `elixir day1.exs 1|2 < input.txt`
  """

  def read_input! do
    IO.stream(:stdio, :line)
    |> Stream.map(fn line ->
      line
      |> String.trim()
      |> String.to_integer()
    end)
  end

  def ans1(input) do
    lam = fn x1, x0, acc ->
      if x1 > x0 do
        {x1, acc + 1}
      else
        {x1, acc}
      end
    end

    Enum.reduce(input, fn
      next, {this, n} -> lam.(next, this, n)
      next, this -> lam.(next, this, 0)
    end)
    |> elem(1)
  end

  def ans2(input) do
    ans1(sums(Enum.to_list(input), 3))
  end

  def sums([_ | rest] = xs, n) do
    Stream.scan(xs, fn a, b -> a + b end)
    |> Enum.at(2)
    |> List.wrap()
    |> Stream.concat(sums(rest, n))
  end

  def sums([], _), do: []
end

case System.argv() do
  ["1"] ->
    IO.inspect(Day1.ans1(Day1.read_input!()))

  ["2"] ->
    IO.inspect(Day1.ans2(Day1.read_input!()))

  _ ->
    IO.puts("Usage: elixir day1.exs 1|2 < input.txt")
end
