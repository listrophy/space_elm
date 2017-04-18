class GamesChannel < ApplicationCable::Channel
  def subscribed
    @game = Game.find(params[:id])
    current_user.game = @game
    current_user.save
    stream_for @game
  end

  def unsubscribed
    current_user.game = nil
    current_user.save
  end
end
