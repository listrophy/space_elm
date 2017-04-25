class GamesChannel < ApplicationCable::Channel
  def subscribed
    @game = Game.find_by id: 1
    stream_for @game
  end

  def unsubscribed
  end
end
