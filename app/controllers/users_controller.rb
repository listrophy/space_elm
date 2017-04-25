class UsersController < ApplicationController
  def show
    render json: {id: current_user.try(:id)}
  end
end
