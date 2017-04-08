class GamesController < ApplicationController
  def index
    render json: Game.all.as_json(only: [:id, :name])
  end
end
