class GamesChannel < ApplicationCable::Channel
  def subscribed
    stream_for game
  end

  def scoreUpdate(data)
    g = game
    g.update score: (g.score + data['score'])
    self.class.broadcast_to g, {score: g.score}
  end

  def unsubscribed
  end

  private

  def game
    Game.first || Game.create
  end
end
