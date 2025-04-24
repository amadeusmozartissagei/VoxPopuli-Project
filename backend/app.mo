import Text "mo:base/Text";
import Time "mo:base/Time";
import HashMap "mo:base/HashMap";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Hash "mo:base/Hash";
import Error "mo:base/Error";
import Blob "mo:base/Blob";

actor AnonymousOpinions {
  // Type for storing media
  type MediaType = {
    #image;
    #video;
  };

  type Media = {
    mediaType: MediaType;
    content: Blob;
  };

  // Type for storing opinions
  type Opinion = {
    id: Nat;
    content: Text;
    timestamp: Time.Time;
    media: ?Media;
  };

  // Stable variables for persistence across upgrades
  stable var nextId: Nat = 0;
  stable var opinionsEntries: [(Nat, Opinion)] = [];
  
  // Use HashMap for more efficient lookups by ID
  let opinions = HashMap.fromIter<Nat, Opinion>(
    opinionsEntries.vals(), 
    10, 
    Nat.equal, 
    Hash.hash
  );

  // Add a simple filter function
  private func containsInappropriateContent(text: Text) : Bool {
    let lowercaseText = Text.toLowercase(text);
    
    // Define a list of inappropriate words to filter
    let inappropriateWords = ["bajingan", "asu", "kontol"];
    
    for (word in inappropriateWords.vals()) {
      if (Text.contains(lowercaseText, #text word)) {
        return true;
      };
    };
    
    return false;
  };

  // Modified postOpinion function with optional media
  public shared func postOpinion(content: Text, media: ?Media) : async Nat {
    if (Text.size(content) == 0) {
      throw Error.reject("Opinion content cannot be empty");
    };
    
    if (containsInappropriateContent(content)) {
      throw Error.reject("Opinion contains inappropriate content");
    };
    
    let newOpinion: Opinion = {
      id = nextId;
      content = content;
      timestamp = Time.now();
      media = media;
    };
    
    opinions.put(nextId, newOpinion);
    nextId += 1;
    return newOpinion.id;
  };

  // Function to upload image
  public shared func postOpinionWithImage(content: Text, imageData: Blob) : async Nat {
    let media: Media = {
      mediaType = #image;
      content = imageData;
    };
    
    return await postOpinion(content, ?media);
  };

  // Function to upload video
  public shared func postOpinionWithVideo(content: Text, videoData: Blob) : async Nat {
    let media: Media = {
      mediaType = #video;
      content = videoData;
    };
    
    return await postOpinion(content, ?media);
  };

  // Query function to get all opinions
  public query func getAllOpinions() : async [Opinion] {
    Iter.toArray(Iter.map(opinions.vals(), func (opinion: Opinion): Opinion { opinion }));
  };

  // Query function to get an opinion by ID
  public query func getOpinion(id: Nat) : async ?Opinion {
    opinions.get(id);
  };
  
  // System functions for data persistence
  system func preupgrade() {
    opinionsEntries := Iter.toArray(opinions.entries());
  };

  system func postupgrade() {
    opinionsEntries := [];
  };
}