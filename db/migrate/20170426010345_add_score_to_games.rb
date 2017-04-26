class AddScoreToGames < ActiveRecord::Migration[5.0]
  def change
    add_column :games, :score, :int, default: 0
  end
end
