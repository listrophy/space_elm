class GamesChannel < ApplicationCable::Channel
  def subscribed
    @game = Game.find(params[:id])
    @game.players << current_user
    stream_for @game
  end

  def unsubscribed
    @game.players.remove current_user
  end
end
